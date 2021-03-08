##' Pranger
##'
##' Pranger bases on unsupervised random forests generated with \code{ranger}
##' to compute proximities between individuals.Two approaches are supported. The
##' first approach of Shi and Horvath (2006) that increases the
##' dissimilarity between two individuals to one if they don't belong to the
##' same terminal node. The second approach of Fouodo et al. (2021) takes the tree
##' depth into account and estimate the dissimilarity between two individuals
##' basing on the length of the minimal path between the terminal nodes they
##' belong to.
##'
##' @param data [\code{data.frame}] A \code{data.frame} of original dataset
##' @param strategy [\code{character}] Name of the resampling strategy to be used.
##'                                    Most be element of
##'                                "boostrepl", "boostwithoutrepl", "boostbayes",
##'                                "unif", "normal", "binomial" or "boostaggr"
##' @param verbose [\code{boolean}] If TRUE, verbose
##' @param ... further parameters to be passed to \code{ranger}
##' @param nb_bootst [\code{integer}] Number of repetitions required to aggregate the
##'                            bootstrap samples. Set to ceiling(sqrt(n)) if not
##'                            provided, with n the number of observations
##' @param aggregation [\code{function}] Aggregation function in the case of
##'                               "boostaggr". Default is the mean function.
##'                               You can pass your on aggregation function
##' @param approach [\code{character}] One of "shi" or "deep". If "shi", then
##' the approach of Shi and  Hovarth (2006) is called to compute dissimilarities.
##' Else, if "deep" the approach of Fouodo et al. (2021) is called.
##'
##' @return [\code{matrix}] Matrix of dissimilarities.
##'                         Note: You can use the function \code{cleandist} to
##'                         convert the dissimilarity matrix into a distance
##'                         matrix
##' @export
##'
##' @examples
##' \dontrun{
##' set.seed(1234)
##' index <- sample(x = 1:150, size = 15, replace = FALSE)
##'   iris_dissimilarities <- pranger(
##'                              data = iris[ , -5],
##'                              strategy = "boostrepl")
##'}
##' @import checkmate
##' @author Cesaire J. K. Fouodo
pranger <- function(
  data,
  strategy,
  nb_bootst = NULL,
  approach = "deep",
  aggregation = mean,
  verbose = FALSE,
  ...
){
  ## Begin of parameter check
  assertions <- makeAssertCollection()
  ## data must be a data.frame
  assert_data_frame(data, types = c("numeric",
                                    "integer",
                                    "character",
                                    "factor"))
  ## strategy must be one of the listed string
  assert_choice(strategy, choices = c("boostrepl",
                                     "boostwithoutrepl",
                                     "boostbayes",
                                     "unif",
                                     "normal",
                                     "binomial",
                                     "boostaggr"
                                     ))
  ## nb_bootst must be either null or integer
  if(strategy != "boostaggr"){
    assert_null(nb_bootst)
  } else {
    if(is.null(nb_bootst)){
      nb_bootst <- ceiling(sqrt(nrow(data)))
    }
  }
  assert_int(nb_bootst)
  ## approach must be one of deep of shi
  assert_choice(approach, choices = c("deep", "shi"))
  ## aggregation must be a function
  assert_function(aggregation)
  ## Verbose must be a flag
  assert_flag(verbose)
  ## Report all assertions
  reportAssertions(assertions)
  ## End of parameter check
  n <- nrow(data)
  nb_bootst <- if(missing(nb_bootst)){
    ceiling(log(nrow(data)))
  } else {
    nb_bootst
  }
  ## Built a two-classes classification problem
  if(verbose){
    cat("Resampling to creating a two-classes problem...\n")
  }
  data <- resampling(data = data,
                     strategy = strategy,
                     nb_bootst = nb_bootst,
                     aggregation = aggregation)
  ## Grow a random forest with ranger
  if(verbose){
    cat("Growing the ranger forest...\n")
  }
  ranger_forest <- ranger(data = data,
                          dependent.variable.name = "yy",
                          ...)
  if(verbose){
    cat("ranger predictions...\n")
  }
  ## Predict the terminal nodes for the original observations only
  ranger_pred <- predict(object = ranger_forest,
                         data = data[1:n, -1],
                         type = "terminalNodes")
  if(verbose){
    cat("Calculating dissimilarities...\n")
  }
  ## Calculate dissimilarities between observations
  forest_dist <- if(approach == "deep"){
    predicted_forest_distance(forest = ranger_forest,
                              predictions = ranger_pred)
  } else {
    shi_ranger_forest(predictions = ranger_pred)
  }
  return(forest_dist)
}
