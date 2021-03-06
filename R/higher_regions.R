#' Compute higher density regions for a given vector confidences
#'
#' @param data [vector(1)] Numeric vector containing data. If x is missing then
#'                         den must be provided, and the HDR is computed from
#'                         the given density.
#' @param probs [vector(1)] Probability coverage required for HDRs
#' @param den [density(1)] Density of data as list with components x and y. See
#'                         package hdrcde for more details.
#' @param h [numeric(1)] Optional bandwidth for calculation of density.
#' @param lambda [numeric(1)] Box-Cox transformation parameter where
#'                            0 <= lambda <= 1
#' @param nn [integer(0)] Number of random numbers used in computing f-alpha
#'                        quantiles.
#' @param all.modes [boolean(1)] Return all local modes or just the global mode?
#'
#' @return [list] List of highest density regions found with correspponding sum-
#'                mary description
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- c(rf(500, 10, 90), rnorm(500, 4, 1))
#' probs <- c(50, 75, 95)
#' hr_res_list <- higher_regions(data = data, probs = probs)
#' @import hdrcde
higher_regions <- function(data,
                           probs = c(95, 75, 50),
                           den = NULL,
                           h = hdrbw(BoxCox(data, lambda), mean(probs)),
                           lambda = 1,
                           nn = 5000,
                           all.modes = FALSE){
  regions_list <- NULL
  region_desc <- lapply(X = probs, function(prob, data){
    res_hdr <- hdr(x = data, prob = prob, den = den,
                   h = h,
                   lambda = lambda,
                   nn = nn,
                   all.modes = all.modes)$hdr
    hdr_bounderies <- matrix(data = res_hdr,
                             nrow = 2, byrow = FALSE)
    rownames(hdr_bounderies) <- c("left", "right")
    colnames(hdr_bounderies) <- paste("hdr", 1:ncol(hdr_bounderies), sep = "")
    region_size <- apply(hdr_bounderies, 2, function(region, observations){
      sum((region["left"] <= observations) & (observations) >= region["right"])
    }, observations = data)
    opt_param <- c(
      "prob" = prob,
      "num_grp" = length(region_size),
      "min_size" = min(region_size))
    regions_list <<- c(regions_list, list(hdr_bounderies))
    return(opt_param)
  }, data = data)
  region_desc <- data.frame(do.call(what = "rbind", args = region_desc))
  return(
    list(
      region_desc = region_desc,
      regions_list = regions_list
    )
  )
}
