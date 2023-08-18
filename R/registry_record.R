#' @title
#' Registry record.
#'
#' @description
#' \href{RegistryTable.html#method-RegistryTable-list_records}{\code{RegistryTable$list_records()}}
#' is the typical way to get a `RegistryRecord`.
#
#' @export
#' @family Registry APIs
RegistryRecord <- R6::R6Class("RegistryRecord",
  private = list(
    cache = list(
      name = NULL,
      columns = NULL,
      values = NULL
    )
  ),
  public = list(
    #' @field id String. Unique identifier.
    id = NULL,

    #' @description
    #' Get the representation of a specific record.
    #'
    #' @param id String. The ID of the target record.
    initialize = function(id) {
      check_string(id)

      self$id <- id
    },

    #' @description
    #' Print a human-readable representation of this record.
    print = function() {
      name <- self$get_name(load_if_missing = FALSE)

      if (!is_null(name)) {
        cat(
          "<RegistryRecord id=", self$id,
          " name=", encodeString(name, quote = '"'),
          ">\n",
          sep = ""
        )
      } else {
        cat("<RegistryRecord id=", self$id, ">\n", sep = "")
      }
    },

    #' @description
    #' (Re-)populate this record instance's cache.
    #'
    #' Future calls to most getters will return immediately without
    #' making a network request.
    #'
    #' Always makes a network request.
    load = function() {
      data <- vacuole_gql_perform("
        query RecordQuery($id: BigInt!) {
          catalogSample(id: $id) {
            name
            catalogSampleColumnDataBySampleId {
              nodes {
                key
                data
              }
            }
            experiment {
              catalogExperimentColumnDefinitionsByExperimentId {
                nodes {
                  type
                  key
                  def
                }
              }
            }
          }
        }
      ", list(id = self$id))$catalogSample

      private$cache$columns <- list()
      type_nodes <- data$experiment$
        catalogExperimentColumnDefinitionsByExperimentId$nodes
      for (col in type_nodes) {
        private$cache$columns[[col$key]] <- RegistryColumn$new(
          col$key,
          col$def, # fixme(maximsmol): deal with defaults
          col$type
        )
      }

      private$cache$values <- list()
      val_nodes <- data$catalogSampleColumnDataBySampleId$nodes
      for (val in val_nodes) {
        col <- private$cache$columns[[val$key]]
        if (is_null(col)) {
          abort(c(
            "Registry value has no corresponding column",
            "x" = paste0("Column name: ", val$key),
            "i" = paste0(
              "Available columns: ",
              oxford_comma(
                private$cache$columns %>%
                  map(function(x) encodeString(x$key, quote = '"')),
                final = "and"
              )
            )
          ))
        }

        try_fetch(
          private$cache$values[[val$key]] <- registry_to_r_literal(
            val$data,
            col$upstream_type$type
          ),
          error = function(cnd) {
            abort(c(
              "Failed to convert record value",
              "i" = paste0(
                "Column ", encodeString(val$key, quote = '"')
              ),
              "i" = paste0(
                "Record ", encodeString(data$name, quote = '"')
              ),
              "x" = paste0(
                "Type: ", jsonlite::toJSON(
                  col$upstream_type$type,
                  auto_unbox = TRUE
                )
              ),
              "x" = paste0(
                "Value: ", jsonlite::toJSON(
                  val$data,
                  auto_unbox = TRUE
                )
              )
            ), parent = cnd)
          }
        )
      }

      for (key in names(private$cache$cols)) {
        if (key %in% private$cache$values) {
          next
        }

        col <- private$cache$cols[[key]]
        if (col$upstream_type$allowEmpty) {
          next
        }

        private$cache$values[[key]] <- RegistryInvalidValue$new("")
      }

      private$cache$name <- data$name
    },

    #' @description
    #' Get the name of this record.
    #'
    #' Names are unique within a table. Names are not globally unique.
    #' Use `id` if a globally unique identifier is required.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the name if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return String value of the name.
    get_name = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$name)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$name
    },

    #' @description
    #' Get the columns of this record's table.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the columns if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return
    #' Named list mapping between column keys and the
    #' corresponding [columns][RegistryColumn].
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
    #' Get this record's values.
    #'
    #' The resulting dictionary is shared between all calls to this
    #' instance's `get_values`.
    #' Make deep copies if independent mutation is desired.
    #'
    #' @param load_if_missing Logical.\cr
    #' If `TRUE`, [load()] the values if not in cache.
    #' If `FALSE`, return `NULL` if not in cache.
    #'
    #' @return
    #' Named list mapping between column keys and the corresponding values.
    get_values = function(load_if_missing = TRUE) {
      check_logical(load_if_missing)

      if (is_null(private$cache$values)) {
        if (!load_if_missing) {
          return(NULL)
        }

        self$load()
      }

      private$cache$values
    }
  )
)
