#' @title
#' Remote latch node (file, directory, or special object).
#'
#' @export
#' @family Latch Data APIs
LDataNode <- R6::R6Class("LDataNode", public = list(
  #' @field remote_path String. Path within the remote Latch Data storage.
  remote_path = NULL,

  #' @description
  #' Get a Latch Data node by path.
  #'
  #' The path is not validated in any way.
  #'
  #' @param remote_path The path of the target node.
  initialize = function(remote_path) {
    check_string(remote_path)

    self$remote_path <- remote_path
  },

  #' @description
  #' Print a human-readable representation of this node.
  print = function() {
    cat(
      "<LDataNode remote_path=",
      encodeString(self$remote_path, quote = '"'),
      ">\n",
      sep = ""
    )
  }
))

#' @title
#' Construct an [Latch Data node object][LDataNode] using a node identifier.
#'
#' @name LDataNode$from_id
#' @family Latch Data APIs
#' @return [LDataNode] referencing the provided ID.
#' @examples
#' LDataNode$from_id("1234")
LDataNode$from_id <- function(node_id) {
  check_string(node_id)
  LDataNode$new(paste0(
    "latch://", node_id, ".node"
  ))
}
