#' predicted_forest_distance Compute dissimilarity matrix over the forest. Node
#' that this function will parallelize over the available CPUs nodes.
#'
#' @param predictions [ranger(1)] An ranger predicted object of terminal nodes
#' @param forest [ranger(1)] A ranger forest
#'
#' @return [matrix(1)] Matrix of dissimilarities over the forest
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1234)
#' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
#' iris.train <- iris[train.idx, ]
#' iris.test <- iris[-train.idx, ]
#' rg.iris <- ranger(Species ~ ., data = iris.train)
#' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
#'  iris_dist <- predicted_forest_distance(
#'    forest = rg.iris,
#'    predictions = pred.iris)
#'}
#' @author Cesaire J. K. Fouodo
#' @import parallel
predicted_forest_distance <- function(
  predictions,
  forest
){
  tree_dist <- mclapply(1:forest$num.trees, function(
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
  }, predictions = predictions, forest = forest)
  ## Reduce and clean dissimilarities
  resumed_dissim <- Reduce('+', tree_dist)
  cleandist <- function(x) {
    x1 <- as.dist(x)
    x1[x1<=0] <- 1e-10
    as.matrix(x1)
  }
  cleaned_dissim <- cleandist(resumed_dissim)
  return(cleaned_dissim)
}
