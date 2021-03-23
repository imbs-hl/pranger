##' Pranger
##'
##' Compute distance between two terminal nodes
##'
##' @param parent_nodes [\code{data.table}] \code{data.table} of parent nodes
##' @param node1 [\code{integer}] A node
##' @param node2 [\code{integer}] A node
##'
##' @return [\code{integer}] distance between node1 and node2
##' @export
##'
##' @examples
##' child_nodes <- list(
##'                     c(1, 0, 3, 5, 0, 0, 7, 0, 0),
##'                     c(2, 0, 4, 6, 0, 0, 8, 0, 0)
##'                   )
##' parent_nodes <- search_parents(ranger_tree = child_nodes)
##' nd <- nodes_distance(
##' parent_nodes = parent_nodes,
##' node1 = 5, node2 = 1)
##' @author Cesaire J. K. Fouodo
##' @import data.table
##' @importFrom utils packageVersion
nodes_distance <- function(
  parent_nodes,
  node1,
  node2
){
  node_dist <- 0
  if(node1 != node2){
    parent1 <- as.matrix(parent_nodes[(node == node1), ])[1, ]
    parent2 <- as.matrix(parent_nodes[(node == node2), ])[1, ]
    tmp <- intersect(parent1, parent2)[1]
    deep1 <- which(parent1 == tmp)[1] - 1
    deep2 <- which(parent2 == tmp)[1] - 1
    node_dist <- deep1 + deep2 - 1
    # node_dist <- if(length(parent1) == length(parent2)){
    #   deep1 + deep2 - 1
    # } else {
    #   (deep1 + deep2 - 1) * sum(parent_nodes$node %in% intersect(parent1, parent2))
    # }
    # node_dist <- (deep1 + deep2 - 1) * (1 / length(intersect(parent1, parent2)))
  }
  return(node_dist)
}
