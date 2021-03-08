##' Pranger
##'
##' Uses predicted terminal nodes of a tree to compute
##' the dissimilarity matrix
##'
##' @param tree_index [\code{integer}] A ranger tree
##' @param forest [\code{ranger}] A ranger object
##' @param predictions [\code{ranger}] Predictions object from class ranger
##'
##' @return [\code{matrix}] A matrix of dissimilarities
##' @export
##'
##' @examples
##' set.seed(1234)
##' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
##' iris.train <- iris[train.idx, ]
##' iris.test <- iris[-train.idx, ]
##' rg.iris <- ranger(Species ~ ., data = iris.train)
##' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
##'  iris_dist <- predicted_tree_distance(
##'    tree_index = 1,
##'    forest = rg.iris,
##'    predictions = pred.iris)
##' @import ranger
##' @import checkmate
##' @author Cesaire J. K. Fouodo
predicted_tree_distance <- function(
  tree_index,
  forest,
  predictions
){
  ## Begin of parameter check
  assertions <- makeAssertCollection()
  ## tree_index must be an integer
  assert_int(tree_index)
  ## forest must be from class ranger.forest
  assert_class(forest, classes = c("ranger"))
  ## predictions must be from class ranger.prediction
  assert_class(predictions, classes = c("ranger.prediction"))
  ## Report all assertions
  reportAssertions(assertions)
  ## End of parameter check
  nodes <- predictions$predictions[ , tree_index]
  tree <- forest$forest$child.nodeIDs[[tree_index]]
  ## Distance between all terminal nodes
  tree_dist <- tree_distance(
    ranger_tree = tree
  )
  tree_dist_rev <- data.table(
    node1 = tree_dist$node2,
    node2 = tree_dist$node1,
    dist = tree_dist$dist
  )
  tree_dist <- rbindlist(list(tree_dist, tree_dist_rev))
  setkeyv(tree_dist, c("node1", "node2"))
  expand_node_pred <- data.table(expand.grid(nodes, nodes))
  colnames(expand_node_pred) <- c("node1", "node2")
  expand_node_pred[ , index := 1:.N]
  expand_node_pred$dist <- 0
  setkeyv(expand_node_pred, c("node1", "node2"))
  ## Distance between individuals
  all_node_dists <- tree_dist[expand_node_pred, allow.cartesian = TRUE]
  all_node_dists[(is.na(dist)), dist := 0]
  all_node_dists$i.dist <- NULL
  all_node_dists <- all_node_dists[order(index, decreasing = FALSE), ]
  dist_mat <- matrix(
    all_node_dists$dist,
    nrow = length(nodes),
    ncol = length(nodes),
    byrow = TRUE
  )
  dist_mat[dist_mat == 0] <- -1
  return(dist_mat)
}
