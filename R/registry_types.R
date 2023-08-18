#' @title
#' Column definition for a Registry table.
#'
#' @description
#' \href{RegistryTable.html#method-RegistryTable-get_columns}{\code{RegistryTable$get_columns()}}
#' is the typical way to get a `RegistryColumn`.
#
#' @export
#' @family Registry APIs
RegistryColumn <- R6::R6Class("RegistryColumn",
  public = list(
    #' @field key
    #' String. Unique identifier within the table. Not globally unique.
    key = NULL,

    #' @field def Default value for new records.
    def = NULL,

    #' @field upstream_type
    #' Raw column type. Used to convert between R values and Registry values.
    upstream_type = NULL,

    #' @description
    #' Create a Registry column definition.
    #'
    #' @param key String. The column key.
    #' @param def The default value.
    #' @param upstream_type The raw column type.
    initialize = function(key, def, upstream_type) {
      check_string(key)
      check_list(def, allow_null = TRUE)
      check_list(upstream_type)

      self$key <- key
      self$def <- def # fixme(maximsmol): deal with defaults
      self$upstream_type <- upstream_type
    },

    #' @description
    #' Print a human-readable representation of this column definition.
    print = function() {
      cat(
        "<RegistryColumn key=", encodeString(self$key, quote = '"'), ">\n",
        sep = ""
      )
    }
  )
)

#' @title
#' [Registry record][RegistryRecord] value that failed validation.
#
#' @export
#' @family Registry APIs
RegistryInvalidValue <- R6::R6Class("RegistryInvalidValue",
  public = list(
    #' @field raw_value
    #' String. User-provided string representation of the invalid value.
    #'
    #' May be `""` (the empty string) if the value is missing but
    #' the column is required.
    raw_value = NULL,

    #' @description
    #' Create a Registry invalid value.
    #'
    #' @param raw_value String. The underlying string value.
    initialize = function(raw_value) {
      check_string(raw_value)
      self$raw_value <- raw_value
    },

    #' @description
    #' Print a human-readable representation of this value.
    print = function() {
      cat(
        "<RegistryInvalidValue raw_value=",
        encodeString(self$raw_value, quote = '"'),
        ">\n",
        sep = ""
      )
    }
  )
)
