##' urf_test computes UNAIR variable importances and uses them for the
##' Janitza et al. (2018) high-dimensional testing procedure.
##'
##' @param data [\code{data.frame}] original data
##' @param target [\code{character}] name of the artificial target variable
##' @param resampling_seed [\code{integer}] resampling seed
##' @param ... more parameters to pass to ranger
##'
##' @return [\code{data.table}] with variable importances and p-values
##'
##' @export
##' @references
##' Janitza, S, Celik, E, Boulesteix, AL. (2018). A computationally fast variable importance test for random forests for high-dimensional data. Adv Data Anal Classif.; doi.org: 10.1007/s11634-016-0276-4
##' Cesaire J. K. Fouodo, Inke R. König Silke Szymczak (2022) Computing variable importance with unsupervised random forests. In review process.
##' @import checkmate
##' @author Cesaire J. K. Fouodo
urf_test <- function(
  data,
  target = "target",
  resampling_seed,
  ...
){
  ## data must be a data.frame
  if(!inherits(data, "data.frame")){
    stop("data must be a data.frame")
  }
  if(!missing(resampling_seed)){
    set.seed(seed = resampling_seed)
  }
  data_resampled <- pranger::resampling(
    data = data,
    strategy = "boostrepl"
  )
  names(data_resampled)[1] <- target
  ## Importance in original case
  ## =========================================================
  ##              Grow random forest in original case
  ## =========================================================
  forest <- ranger(data = data_resampled,
                   dependent.variable.name = target,
                   importance = "impurity",
                   ...
  )
  imp <- ranger::importance(x = forest)
  ## =========================================================
  ##              Create the null case scenario
  ## =========================================================
  ##
  if(!missing(resampling_seed)){
    set.seed(seed = resampling_seed + 1)
  }
  data_null <- lapply(data, function(i){
    sample(i)
  })
  data_null <- data.frame(data_null)
  if(!missing(resampling_seed)){
    set.seed(seed = resampling_seed + 2)
  }
  data_null_resampled <- pranger::resampling(
    data = data_null,
    strategy = "boostrepl"
  )
  names(data_null_resampled)[1] <- target
  forest_null <- ranger(data = data_null_resampled,
                        dependent.variable.name = target,
                        importance = "impurity",
                        ...
  )
  imp_null <- ranger::importance(x = forest_null)
  ## =========================================================
  ##              Compute pvalue
  ## =========================================================
  imp_diff <- imp - imp_null
  dist_null <- c(imp_diff[imp_diff < 0],
                 imp_diff[imp_diff == 0],
                 -imp_diff[imp_diff < 0])
  if(length(imp_diff) < 100){
    warning("There is not enough negativ values for the null distributions")
  }
  pvalue <- sapply(imp_diff, function(x){
    1 - mean(dist_null <= x)
  })
  return(data.table(
    importance = imp_diff,
    pvalue = pvalue
  ))
}
