#' predicted_forest_distance_parall computes dissimilarities for one tree in the forest
#'
#' @param index_tree [integer(1)] Index of concerned tree
#' @param predictions [ranger(1)] An ranger predicted object of terminal nodes
#' @param forest [ranger(1)] A ranger forest
#'
#' @return [matrix(1)] Matrix of dissimilarities for the concerned tree
#' @export
#'
#' library(ranger)
#' set.seed(1234)
#' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
#' iris.train <- iris[train.idx, ]
#' iris.test <- iris[-train.idx, ]
#' rg.iris <- ranger(Species ~ ., data = iris.train)
#' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
#'  iris_dist <- predicted_forest_distance_parall(
#'    forest = rg.iris,
#'    predictions = pred.iris)
#' @author Cesaire J. K. Fouodo
#' @import stats
predicted_forest_distance_parall <- function(
  index_tree,
  predictions,
  forest
){
  ## Source code for new distance
  cat(sprintf("Processing tree %s...\n", index_tree))
  data_dist <- predicted_tree_distance(
    tree_index = index_tree,
    forest = forest,
    predictions = predictions)
  return(data_dist)
}
