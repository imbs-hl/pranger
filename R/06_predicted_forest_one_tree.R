##' Pranger
##'
##' Computes dissimilarities for one tree of the forest
##'
##' @param index_tree [\code{integer}] Index of concerned tree
##' @param predictions [\code{ranger}] An ranger predicted object of terminal
##'                                    nodes
##' @param forest [\code{ranger}] A ranger forest
##'
##' @return [\code{matrix}] Matrix of dissimilarities for the concerned tree
##' @export
##'
##' @examples
##' \dontrun{
##' library(ranger)
##' set.seed(1234)
##' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
##' iris.train <- iris[train.idx, ]
##' iris.test <- iris[-train.idx, ]
##' rg.iris <- ranger(Species ~ ., data = iris.train)
##' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
##'  iris_dist <- predicted_forest_distance_parall(
##'    forest = rg.iris,
##'    predictions = pred.iris)
##'    }
##' @author Cesaire J. K. Fouodo
##' @import stats
predicted_forest_distance_parall <- function(
  index_tree,
  predictions,
  forest
){
  cat(sprintf("Tree %s...\n", index_tree))
  data_dist <- predicted_tree_distance(
    tree_index = index_tree,
    forest = forest,
    predictions = predictions)
  return(data_dist)
}
