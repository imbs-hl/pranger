##' Pranger
##'
##' Computes dissimilarities between  individuals based on the approach of
##' Shi and Hovarth (2006) over a forest. For each tree, dissimilarity between
##' two individuals is increased to one if they don't belong to the same
##' terminal nodes. Dissimilarities over the forest is obtained by summing
##' dissimilarities obtained from each tree.
##'
##' @param predictions [\code{predict.ranger}] Terminal nodes predicted by
##'                                    \code{predict.ranger}
##' @param init_dist [\code{integer}] Initial distance between in-of-bag nodes
##'
##' @return [\code{matrix}] Dissimilarity matrix according to Shi and Hovarth (2006)
##' @export
##'
##' @examples
##' library(ranger)
##' set.seed(1234)
##' train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
##' iris.train <- iris[train.idx, ]
##' iris.test <- iris[-train.idx, ]
##' rg.iris <- ranger(Species ~ ., data = iris.train, num.trees = 5)
##' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
##' shi_dist <- shi_ranger_forest(predictions = pred.iris)
shi_ranger_forest <- function(predictions, init_dist = 0){
  forest_dist <- lapply(1:ncol(predictions$predictions), function(tree){
    predicted_nodes <- predictions$predictions[ , tree]
    tmp <- lapply(predicted_nodes, function(i){
      res <- as.numeric(predicted_nodes != i)
      res[is.na(res)] <- init_dist
      return(res)
    })
    dist_shi_ranger <- Reduce(f = "cbind", x = tmp)
    return(dist_shi_ranger)
  })
  forest_dist <- Reduce(f = "+", x = forest_dist)
  forest_dist <- cleandist(forest_dist)
  return(forest_dist)
}
