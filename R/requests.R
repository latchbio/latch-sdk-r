sdk_domain <- Sys.getenv("LATCH_SDK_DOMAIN", unset = "latch.bio")
version_str <- packageVersion("latch")

default_useragent <- (
  request("https://google.com") %>% req_user_agent()
)$options$useragent
latch_useragent <- paste0("latch/", version_str, ", ", default_useragent)

#' Initialize an authorized API request
#'
#' Adds default options to [httr2::request()]:
#' - User agent
#' - Retry config
#' - Timeout
#' - Authorization using [auth_header()]
#'
#' @keywords internal
#' @param url String. The request target.
#' @return Pre-configured [httr2::request()].
#' @examples
#' \dontrun{
#' latch_request("https://vacuole.latch.bio") %>%
#'   httr2::req_dry_run()
#' }
latch_request <- function(url) {
  check_string(url)

  request(url) %>%
    req_user_agent(latch_useragent) %>%
    # req_retry(max_tries = 10, max_seconds = 60) %>%
    # req_timeout(60) %>%
    req_headers(Authorization = auth_header())
}

#' Make a request to Latch's internal Vacuole GraphQL API
#'
#' This API is intended for internal use only:
#' - We will provide no documentation or support
#' - There are no backwards compatibility guarantees
#'
#' @keywords internal
#' @return JSON object containing the response
#' @examples
#' \dontrun{
#' vacuole_gql_perform("query WhoAmI { accountInfoCurrent { id } }")
#' }
vacuole_gql_perform <- function(...) {
  latch_request(paste0("https://vacuole.", sdk_domain, "/graphql")) %>%
    graphql_body(...) %>%
    graphql_perform()
}
