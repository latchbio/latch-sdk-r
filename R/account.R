#' @title
#' User or team workspace. Can be used to fetch related resources.
#'
#' @description
#' [Account$current()] is the typical way of creating an `Account`
#'
#' If the current request signer (CLI user or execution context)
#' lacks permissions to fetch some information, the corresponding operations
#' will act as if the information does not exist. Update operations will usually
#' produce errors.
#'
#' @export
#' @family Accounts APIs
#' @examples
#' Account$new("1")
Account <- R6::R6Class("Account",
  private = list(
    cache = list(
      display_name = NULL,
      registry_projects = NULL
    )
  ),
  public = list(
    #' @field id String. Unique identifier.
    id = NULL,

    #' @description
    #' Get the representation of a specific account.
    #'
    #' @param id String. The ID of the target account.
    initialize = function(id) {
      check_string(id)

      self$id <- id
    },

    #' @description
    #' Print a human-readable representation of this account.
    #' @examples
    #' acc <- Account$current()
    #' acc$print()
    print = function() {
      display_name <- self$get_display_name(load_if_missing = FALSE)

      if (!is_null(display_name)) {
        cat(
          "<Account id=", self$id,
          " display_name=", encodeString(display_name, quote = '"'),
          ">\n",
          sep = ""
        )
      } else {
        cat("<Account id=", self$id, ">\n", sep = "")
      }
    },

    #' @description
    #' (Re-)populate this account instance's cache.
    #'
    #' Future calls to most getters will return immediately without
    #' making a network request.
    #'
    #' Always makes a network request.
    #' @examplesIf interactive()
    #' acc <- Account$current()
    #' acc$print()
    #' acc$load()
    #' acc$print()
    load = function() {
      data <- vacuole_gql_perform("
        query AccountQuery($ownerId: BigInt!) {
          accountInfo(id: $ownerId) {
            userInfoByAccountId {
              name
            }
            teamInfoByAccountId {
              displayName
            }
            catalogProjectsByOwnerId(condition: { removed: false }) {
              nodes {
                id
                displayName

                catalogExperimentsByProjectId(condition: { removed: false }) {
                  nodes {
                    id
                    displayName
                  }
                }
              }
            }
          }
        }
      ", list(ownerId = self$id))$accountInfo

      registry_projects <- list()
      for (x in data$catalogProjectsByOwnerId$nodes) {
        cur <- RegistryProject$new(x$id)

        tables <- list()
        for (tbl in x$catalogExperimentsByProjectId$nodes) {
          cur_t <- RegistryTable$new(tbl$id)
          cur_t$.__enclos_env__$private$cache$display_name <- tbl$displayName

          tables <- c(
            tables,
            cur_t
          )
        }

        cur$.__enclos_env__$private$cache$display_name <- x$displayName
        cur$.__enclos_env__$private$cache$tables <- tables

        registry_projects <- c(
          registry_projects,
          cur
        )
      }

      user_name <- data$userInfoByAccountId$name
      team_name <- data$teamInfoByAccountId$displayName

      private$cache$display_name <- user_name %||% team_name
      private$cache$registry_projects <- registry_projects
    },

    #' @description
    #' Get the display name of this account.
    #'
    #' This is an opaque string that can contain any valid Unicode data.
    #'
    #' Display names are *not unique* and *must never be used as identifiers*.
    #' Use `id` instead.
    #'
    #' For users, this is the user's full name as reported by the authenitcation
    #' provider.
    #'
    #' For teams, this is the team display name.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the display name if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return String value of the display name.
    #' @examplesIf interactive()
    #' acc <- Account$current()
    #' acc$get_display_name()
    get_display_name = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$display_name)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$display_name
    },

    #' @description
    #' List Registry projects owned by this workspace.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the project list if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return List of [projects][RegistryProject] owned by this workspace.
    #' @examplesIf interactive()
    #' acc <- Account$current()
    #' acc$list_registry_projects()
    list_registry_projects = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$registry_projects)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$registry_projects
    }
  )
)

#' @title
#' Get current account
#'
#' @description
#' In an execution context, this is the workspace in which the execution
#' was run.
#'
#' In the CLI context (when running `latch` commands) this is the
#' current setting of `latch workspace`, which defaults to the user's personal
#' workspace.
#'
#' @name Account$current
#' @family Accounts APIs
#' @return [Account] for the current workspace.
#' @examplesIf interactive()
#' Account$current()
Account$current <- function() {
  data <- vacuole_gql_perform("
    query accountInfoQuery {
      accountInfoCurrent {
        id
      }
    }
  ")
  Account$new(data$accountInfoCurrent$id)
}
