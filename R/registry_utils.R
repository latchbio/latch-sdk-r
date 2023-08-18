.registry_to_r_literal <- function(registry_literal, registry_type) {
  check_list(registry_literal)
  check_list(registry_type)

  array_type <- registry_type$array
  if (!is_null(array_type)) {
    check_list(registry_literal)

    return(
      registry_literal %>%
        map(function(x) .registry_to_r_literal(x, array_type))
    )
  }

  union_type <- registry_type$union
  if (!is_null(union_type)) {
    check_string(registry_literal$tag)

    tag <- registry_literal$tag
    sub_type <- union_type[[tag]]
    if (is.null(sub_type)) {
      abort(c(
        "Union tag is invalid",
        "x" = paste0("Tag: ", encodeString(tag, quote = '"')),
        "i" = paste0(
          "Expected one of: ", oxford_comma(
            names(union_type) %>%
              map(function(x) encodeString(x, quote = '"'))
          )
        )
      ))
    }

    return(.registry_to_r_literal(registry_literal$value, sub_type))
  }

  check_bool(registry_literal$valid)
  if (!registry_literal$valid) {
    check_string(registry_literal$rawValue)
    return(RegistryInvalidValue$new(registry_literal$rawValue))
  }

  prim <- registry_type$primitive
  if (prim == "null") {
    check_null(registry_literal$value)
    return(NULL)
  }
  if (prim == "string") {
    check_string(registry_literal$value)
    return(as.character(registry_literal$value))
  }
  if (prim == "number") {
    check_number_decimal(registry_literal$value)
    return(as.double(registry_literal$value))
  }
  if (prim == "integer") {
    check_number_whole(registry_literal$value)
    return(as.integer(registry_literal$value))
  }
  if (prim == "boolean") {
    check_logical(registry_literal$value)
    return(as.logical(registry_literal$value))
  }
  if (prim == "date") {
    check_string(registry_literal$value)
    return(
      clock::year_month_day_parse(registry_literal$value)
    )
  }
  if (prim == "datetime") {
    check_string(registry_literal$value)
    return(
      clock::sys_time_parse_RFC_3339(
        registry_literal$value,
        precision = "millisecond"
      )
    )
  }
  if (prim == "link") {
    check_string(registry_literal$value$sampleId)
    return(RegistryRecord$new(registry_literal$value$sampleId))
  }
  if (prim == "enum") {
    check_string(registry_literal$value)

    levels <- registry_type$members
    if (!(registry_literal$value %in% levels)) {
      abort(c(
        "Value is not part of the enumeration",
        "i" = paste0(
          "Acceptable values are: ",
          oxford_comma(
            levels %>% map(function(x) encodeString(x, quote = '"')),
            final = "and"
          )
        )
      ))
    }

    return(factor(x = registry_literal$value, levels = levels))
  }
  if (prim == "blob") {
    check_string(registry_literal$value$ldataNodeId)
    return(LDataNode$from_id(registry_literal$value$ldataNodeId))
  }

  abort(c(
    "Unknown type",
    "x" = jsonlite::toJSON(registry_type, auto_unbox = TRUE)
  ))
}

#' Adapt Registry value into the corresponding R value.
#'
#' @keywords internal
#' @param registry_literal
#' JSON object containing the raw Registry literal value.
#' @param registry_type
#' JSON object containing the Registry type definition.
#' @return R equivalent of the registry literal.
registry_to_r_literal <- function(registry_literal, registry_type) {
  check_list(registry_literal)
  check_list(registry_type)

  try_fetch(
    .registry_to_r_literal(registry_literal, registry_type),
    error = function(cnd) {
      abort(
        "Registry value does not match column type",
        parent = cnd
      )
    }
  )
}
