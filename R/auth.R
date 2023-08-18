#' Authorize API calls
#'
#' Valid in workflow executions and after signing in using Latch CLI
#'
#' @keywords internal
#' @return String value of the `Authorization` HTTP header.
#' @examplesIf interactive()
#' httr2::request("https://vacuole.latch.bio") %>%
#'   httr2::req_headers(Authorization = auth_header()) %>%
#'   httr2::req_dry_run()
auth_header <- function() {
  exec_token <- Sys.getenv("FLYTE_INTERNAL_EXECUTION_ID", unset = NA)
  if (!is_na(exec_token)) {
    return(paste0("Latch-Execution-Token ", exec_token))
  }

  sdk_token <- try_fetch(
    readr::read_file("~/.latch/token"),
    error = function(cnd) {
      if (stringr::str_detect(cnd$message, "does not exist")) {
        return(NA)
      }
      abort(c(
        "Can't read latch CLI token file",
        "x" = "Failed with an unknown error"
      ), parent = cnd)
    }
  )
  if (!is_na(sdk_token)) {
    return(paste0("Latch-SDK-Token ", sdk_token))
  }

  abort(c(
    "Can't find SDK credentials.",
    "x" = "Execution token environment variable is not set.",
    "x" = "CLI token file does not exist.",
    "i" = "Did you forget to run `latch login`?"
  ))
}
