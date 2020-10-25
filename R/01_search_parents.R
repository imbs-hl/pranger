#' Search parent nodes of each terminal node from a tree
#'
#' @param ranger_tree [ranger.tree] A ranger tree\cr
#'
#' @return [\code{data.table(1)}]
#'   A \code{data.table} containing parent nodes of each terminal nodes
#' @export
#'
#' @examples
#'child_nodes <- list(
#'                     c(1, 0, 3, 5, 0, 0, 7, 0, 0),
#'                     c(2, 0, 4, 6, 0, 0, 8, 0, 0)
#'                   )
#'  parent_nodes <- search_parents(ranger_tree = child_nodes)
#' @author Cesaire J. K. Fouodo
#' @import data.table
#' @importFrom utils packageVersion
search_parents <- function(
  ranger_tree
){
  ## n_node also represents the deeper node
  n_nodes <- length(ranger_tree[[1]]) - 1
  parent_daughter <- data.table(
    parent = 0:n_nodes,
    daughter_left = ranger_tree[[1]],
    daughter_right = ranger_tree[[2]]
  )
  parent_daughter_molten <- melt(
    data = parent_daughter,
    id.vars = "parent",
    value.name = "child")
  current_parent <- n_nodes
  parents <- NULL
  while(current_parent != 0){
    parents <- c(parents, current_parent)
    current_parent <- parent_daughter_molten[(child == current_parent), parent]
  }
  parents <- c(parents, 0)
  ## Get terminal nodes over the tree
  terminal_nodes <- parent_daughter[(
    daughter_left + daughter_right == 0), parent]

  parent_nodes <- matrix(rep(0, length(terminal_nodes) * length(parents)),
                         nrow = length(terminal_nodes),
                         ncol = length(parents)
  )
  ## Set the first column to terminal nodes
  parent_nodes[ , 1] <- terminal_nodes
  res <- unlist(
    lapply(
      2:length(parents),
      function(i){
        tmp <- parent_nodes[ , i - 1]
        parent_index_left <- match(tmp[tmp != 0], parent_daughter$daughter_left)
        parent_index_right <- match(tmp[tmp != 0], parent_daughter$daughter_right)
        parent_index_left[is.na(parent_index_left)] <- 0
        parent_index_right[is.na(parent_index_right)] <- 0
        parent_index <- parent_index_left + parent_index_right
        tmp[tmp != 0] <- parent_index
        parent_nodes[(tmp != 0), i] <<- parent_daughter[(tmp), parent]
      }))
  parent_nodes <- data.table(parent_nodes)
  ## Set column names
  names(parent_nodes) <- c("node", paste("parent",
                                         1:(ncol(parent_nodes) - 1),
                                         sep = "")
  )
  return(parent_nodes)
}
