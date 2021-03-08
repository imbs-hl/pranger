##' Pranger
##'
##' Compute distances between all terminal nodes for a given tree
##'
##' @param ranger_tree [\code{ranger.tree}] A ranger tree
##'
##' @return [\code{matrix}] A matrix of dissimilarities
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
##' tree_distance(
##' ranger_tree = child_nodes
##' )
##' @author Cesaire J. K. Fouodo
##' @importFrom gtools combinations
##' @importFrom utils packageVersion
tree_distance <- function(
  ranger_tree
){
  ## Search parents
  parent_nodes <- search_parents(ranger_tree = ranger_tree)
  terminal_nodes <- parent_nodes$node
  options(expressions=1e5)
  expand_node <- combinations(
    n = length(terminal_nodes),
    r = 2,
    v = terminal_nodes)
  expand_node <- t(expand_node)
  ## Pairwise distances
    tree_dist <- apply(
      X = expand_node,
      MARGIN = 2,
      FUN = function(node_pair){
        nodes_distance(
          parent_nodes = parent_nodes,
          node1 = node_pair[1], node2 = node_pair[2]
        )
      }
    )
  expand_node <- t(rbind(expand_node, tree_dist))
  colnames(expand_node) <- c("node1", "node2", "dist")
  return(data.table(expand_node))
}
