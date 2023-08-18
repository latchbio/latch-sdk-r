#' @title
#' Registry project (folder containing [Registry tables][RegistryTable]).
#'
#' @description
#' \href{Account.html#method-Account-list_registry_projects}{\code{Account$list_registry_projects()}}
#' is the typical way to get a `RegistryProject`.
#'
#' @export
#' @family Registry APIs
RegistryProject <- R6::R6Class("RegistryProject",
  private = list(
    cache = list(
      display_name = NULL,
      tables = NULL
    )
  ),
  public = list(
    #' @field id String. Unique identifier.
    id = NULL,

    #' @description
    #' Get the representation of a specific project.
    #'
    #' @param id String. The ID of the target project.
    initialize = function(id) {
      check_string(id)

      self$id <- id
    },

    #' @description
    #' Print a human-readable representation of this project.
    print = function() {
      display_name <- self$get_display_name(load_if_missing = FALSE)

      if (!is_null(display_name)) {
        cat(
          "<RegistryProject id=", self$id,
          " display_name=", encodeString(display_name, quote = '"'),
          ">\n",
          sep = ""
        )
      } else {
        cat("<RegistryProject id=", self$id, ">\n", sep = "")
      }
    },

    #' @description
    #' (Re-)populate this project instance's cache.
    #'
    #' Future calls to most getters will return immediately without
    #' making a network request.
    #'
    #' Always makes a network request.
    load = function() {
      data <- vacuole_gql_perform("
        query ProjectQuery($id: BigInt!) {
          catalogProject(id: $id) {
            displayName
            catalogExperimentsByProjectId(
              condition: { projectId: $id, removed: false }
            ) {
              nodes {
                id
                displayName
              }
            }
          }
        }
      ", list(id = self$id))$catalogProject

      tables <- list()
      for (x in data$catalogExperimentsByProjectId$nodes) {
        cur <- RegistryTable$new(x$id)
        cur$.__enclos_env__$private$cache$display_name <- x$displayName

        tables <- c(
          tables,
          cur
        )
      }

      private$cache$display_name <- data$displayName
      private$cache$tables <- tables
    },

    #' @description
    #' Get the display name of this project.
    #'
    #' This is an opaque string that can contain any valid Unicode data.
    #'
    #' Display names are *not unique* and *must never be used as identifiers*.
    #' Use `id` instead.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the display name if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return String value of the display name.
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
    #' List tables contained in this project.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the table list if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return List of [tables][RegistryTable] contained in this project.
    list_tables = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$tables)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$tables
    }
  )
)
