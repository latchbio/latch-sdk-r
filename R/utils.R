check_null <- function(x,
                       ...,
                       allow_na = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && (is_null(x) || allow_na && is_na(x))) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "`NULL`",
    ...,
    allow_na = allow_na,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

check_list <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && (
    is_list(x) ||
      (allow_na && is_na(x)) ||
      (allow_null && is_null(x))
  )) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a list",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
