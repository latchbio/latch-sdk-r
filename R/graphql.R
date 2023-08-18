#' Prepare a GraphQL request payload
#'
#' @keywords internal
#' @param req [httr2::request()] to modify
#' @param query String value of the GraphQL document
#' @param variables Named list of JSON values for the query parameters
#' @return [httr2::req_body_json()] with the payload
#' @examples
#' \dontrun{
#' httr2::request("https://gql.example.com") %>%
#'   graphql_body(
#'     "query Hello($username: String!) { hello(name: $username) }",
#'     variables = list(username = "John")
#'   ) %>%
#'   httr2::req_dry_run()
#' }
graphql_body <- function(req, query, variables = NULL) {
  check_string(query)
  check_list(variables, allow_null = TRUE)

  data <- list(
    "query" = query
  )

  if (!is_null(variables)) {
    data$variables <- variables
  }

  req_body_json(req, data, auto_unbox = TRUE)
}

#' Get and process the response to a prepared GraphQL request
#'
#' 1. Makes the HTTP request
#' 1. Aborts on any GraphQL errors after pretty-printing them
#' 1. Returns the `data` field of the response
#'
#' @keywords internal
#' @param req [httr2::request()] to perform
#' @return JSON object containing the response
#' @examples
#' \dontrun{
#' latch_request(paste0("https://vacuole.", sdk_domain, "/graphql")) %>%
#'   graphql_body("query WhoAmI { accountInfoCurrent { id } }") %>%
#'   graphql_perform()
#' }
graphql_perform <- function(req) {
  # check_request(req) # todo(maximsmol)

  data <- req %>%
    req_error(is_error = function(resp) {
      status <- resp_status(resp)
      if (status == 400) {
        return(FALSE)
      }

      400 <= status && status < 600
    }) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)

  if (!is_null(data$errors)) {
    error_data <- vector()
    error_data[""] <- "GraphQL query failed with errors"

    for (error in data$errors) {
      locations_str <- error$locations %>%
        map(function(x) paste0(x$line, ":", x$column)) %>%
        paste(sep = ", ")

      path_str <- paste(error$path, sep = ".")

      msg <- paste0(error$message, " (at ", locations_str, ")")
      if (length(path_str) > 0) {
        msg <- paste0(msg, " (", path_str, ")")
      }

      error_data["x"] <- msg
    }

    abort(error_data)
  }

  data$data
}
