##' Pranger
##'
##' Given a tree, this function computes dissimilarities between  individuals
##' based on the approach of Shi and Hovarth (2006). That is, dissimilarity
##' between two individuals is increased to one if they don't belong to the same
##' terminal nodes
##'
##' @param tree [\code{integer}] Tree index for which the dissimilarity matrix is required
##' @param predictions [\code{predict.ranger}] Terminal nodes predicted by
##'                                    \code{predict.ranger}
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
##' rg.iris <- ranger(Species ~ ., data = iris.train)
##' pred.iris <- predict(rg.iris, data = iris.test, type = "terminalNodes")
##' shi_dist <- shi_ranger_one_tree(tree = 1,
##'                  predictions = pred.iris)
shi_ranger_one_tree <- function(tree = 1, predictions){
  predicted_nodes <- predictions$predictions[ , tree]
  tmp <- lapply(predicted_nodes, function(i){
    as.numeric(predicted_nodes != i)
  })
  dist_shi_ranger <- Reduce(f = "cbind", x = tmp)
  return(dist_shi_ranger)
}
