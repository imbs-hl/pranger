#' Title
#'
#' @param data [data.frame(1)] A \code{data.frame} of original dataset
#' @param strategy [character(1)] Name of strategy to be used. Most be element of
#'                                "boostrepl", "boostwithoutrepl", "boostbayes",
#'                                "unif", "normal" or "binomial"
#' @param ... further parameters to be passed to \code{ranger}
#'
#' @return [matrix(1)] Matrix of dissimilarities
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1234)
#' index <- sample(x = 1:150, size = 15, replace = FALSE)
#'   iris_dissimilarities <- pranger(
#'                              data = iris[ , -5],
#'                              strategy = "boostrepl")
#'}
#' @author Cesaire J. K. Fouodo
#' @importFrom utils packageVersion
pranger <- function(
  data,
  strategy,
  ...
){
  n <- nrow(data)
  ## Built a two-classes classification problem
  data <- resampling(data = data,
                     strategy = strategy)
  ## Grow a random forest with ranger
  ranger_forest <- ranger(data = data,
                          dependent.variable.name = "yy",
                          ...)
  ## Predict the terminal nodes for the original observations only
  ranger_pred <- predict(object = ranger_forest,
                         data = data[1:n, -1],
                         type = "terminalNodes")
  ## Calculate dissimilarities
  forest_dist <- predicted_forest_distance(forest = ranger_forest,
                                           predictions = ranger_pred)
  return(forest_dist)
}
