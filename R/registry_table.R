#' @title
#' Registry table. Contains [records][RegistryRecord].
#'
#' @description
#' \href{RegistryProject.html#method-RegistryProject-list_tables}{\code{RegistryProject$list_tables()}}
#' is the typical way to get a `RegistryTable`.
#
#' @export
#' @family Registry APIs
RegistryTable <- R6::R6Class("RegistryTable",
  private = list(
    cache = list(
      display_name = NULL,
      columns = NULL
    )
  ),
  public = list(
    #' @field id String. Unique identifier.
    id = NULL,

    #' @description
    #' Get the representation of a specific table.
    #'
    #' @param id String. The ID of the target table.
    initialize = function(id) {
      check_string(id)
      self$id <- id
    },

    #' @description
    #' Print a human-readable representation of this table.
    print = function() {
      display_name <- self$get_display_name(load_if_missing = FALSE)

      if (!is_null(display_name)) {
        cat(
          "<RegistryTable id=", self$id,
          " display_name=", encodeString(display_name, quote = '"'),
          ">\n",
          sep = ""
        )
      } else {
        cat("<RegistryTable id=", self$id, ">\n", sep = "")
      }
    },

    #' @description
    #' (Re-)populate this table instance's cache.
    #'
    #' Future calls to most getters will return immediately without
    #' making a network request.
    #'
    #' Always makes a network request.
    load = function() {
      data <- vacuole_gql_perform("
        query TableQuery($id: BigInt!) {
          catalogExperiment(id: $id) {
            displayName
            catalogExperimentColumnDefinitionsByExperimentId {
              nodes {
                key
                type
                def
              }
            }
          }
        }
      ", list(id = self$id))$catalogExperiment

      columns <- list()
      for (x in data$catalogExperimentColumnDefinitionsByExperimentId$nodes) {
        cur <- RegistryColumn$new(x$key, x$def, x$type)

        columns[[x$key]] <- cur
      }

      private$cache$display_name <- data$displayName
      private$cache$columns <- columns
    },

    #' @description
    #' Get the display name of this table.
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
    #' Get the columns of this table.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the columns if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return Named list of [columns][RegistryColumn] of this table.
    get_columns = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$columns)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$columns
    },

    #' @description
    #' List records contained in this table.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the records if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @param page_size Number of records to return at a time
    #' @return
    #' [coro::generator()] returning pages of [records][RegistryRecord] from
    #' this table.
    list_records = function(page_size = 100) {
      check_number_whole(page_size)

      cols <- self$get_columns()

      data <- vacuole_gql_perform("
        query TableQuery($id: BigInt!) {
          catalogExperiment(id: $id) {
            allSamples {
              nodes {
                sampleId
                sampleName
                sampleDataKey
                sampleDataValue
              }
            }
          }
        }
      ", list(id = self$id))$catalogExperiment$allSamples$nodes

      record_names <- list()
      record_values <- list()

      for (node in data) {
        check_string(node$sampleId)
        check_string(node$sampleName)
        check_string(node$sampleDataKey)

        record_names[[node$sampleId]] <- node$sampleName
        record_values[[node$sampleId]] <- (
          record_values[[node$sampleId]] %||% list()
        )

        col <- cols[[node$sampleDataKey]]
        if (is_null(col)) {
          # todo(maximsmol): throw error?
          next
        }

        try_fetch(
          record_values[[node$sampleId]][[col$key]] <- registry_to_r_literal(
            node$sampleDataValue,
            col$upstream_type$type
          ),
          error = function(cnd) {
            abort(c(
              "Failed to convert record value",
              "i" = paste0(
                "Column ", encodeString(node$sampleDataKey, quote = '"')
              ),
              "i" = paste0(
                "Record ", encodeString(node$sampleName, quote = '"')
              ),
              "x" = paste0(
                "Type: ", jsonlite::toJSON(
                  col$upstream_type$type,
                  auto_unbox = TRUE
                )
              ),
              "x" = paste0(
                "Value: ", jsonlite::toJSON(
                  node$sampleDataValue,
                  auto_unbox = TRUE
                )
              )
            ), parent = cnd)
          }
        )
      }

      coro::generator(function() {
        page <- list()
        for (id in names(record_values)) {
          vals <- record_values[[id]]

          for (col in cols) {
            if (!is_null(vals[[col$key]])) {
              next
            }

            if (col$upstream_type$allowEmpty) {
              next
            }

            vals[[col$key]] <- RegistryInvalidValue$new("")
          }

          cur <- RegistryRecord$new(id)
          cur$.__enclos_env__$private$cache$name <- record_names[[id]]
          cur$.__enclos_env__$private$cache$values <- vals
          cur$.__enclos_env__$private$cache$columns <- cols
          page[[id]] <- cur

          if (length(page) == page_size) {
            yield(page)
            page <- list()
          }
        }

        if (length(page) > 0) {
          yield(page)
        }
      })()
    }
  )
)
