#' Bases on highest density regions to re-sample on sparse region
#'
#' @param data [vector(1)] Numeric vector containing data. If x is missing then
#'                         den must be provided, and the HDR is computed from
#'                         the given density.
#' @param probs [vector(1)] Probability coverage required for HDRs
#' @param min_region_size [integer(1)] Minimal region size.
#' @param nb_bootst [integer] Number of repetitions required to aggregate the
#'                             bootstrap samples. Set to ceiling(sqrt(n)) if not
#'                             provided, with n the number of observations
#' @param aggregation [function] Aggregation function in the case of "boostaggr".
#'                               Default is the mean function. You can pass your
#'                               on aggregation function
#' @param den [density(1)] Density of data as list with components x and y. See
#'                         package hdrcde for more details.
#' @param h [numeric(1)] Optional bandwidth for calculation of density.
#' @param lambda [numeric(1)] Box-Cox transformation parameter where
#'                            0 <= lambda <= 1
#' @param nn [integer(0)] Number of random numbers used in computing f-alpha
#'                        quantiles.
#' @param all.modes [boolean(1)] Return all local modes or just the global mode?
#'
#' @return [vector(1)] a vector with both original and synthetic data
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- c(rf(1000, 10, 90),
#'          rnorm(1000, 4, 0.5),
#'          rnorm(1000, 7, 0.5),
#'          rnorm(1000, 10, 0.5))
#' probs <- c(95, 75, 50)
#' set.seed(123)
#' hdr_rspl <- hdr_resampling (data = data,
#'                            probs = probs,
#'                            min_region_size = 100,
#'                            nb_bootst = ceiling(sqrt(length(data))),
#'                            aggregation = median)
#' @import hdrcde
#' @author Cesaire J. K. Fouodo
#' @importFrom utils packageVersion
hdr_resampling <- function(data,
                           probs = c(95, 75, 50),
                           min_region_size,
                           nb_bootst = ceiling(sqrt(length(data))),## Not yet used
                           aggregation,
                           den = NULL,
                           h = hdrbw(BoxCox(data, lambda), mean(probs)),
                           lambda = 1,
                           nn = 5000,
                           all.modes = FALSE){
  highest_reg <- highest_regions(data = data,
                                 probs = probs,
                                 min_region_size = min_region_size,
                                 den = den,
                                 h = h,
                                 lambda = lambda,
                                 nn = nn,
                                 all.modes = all.modes)
  resampled <- if(ncol(highest_reg) == 1){
    ## Only one highest density region, saying one mode
    return(sample(
      x = data,
      size = length(data),
      replace = TRUE
    ))
  } else {
    ## More than one highest density region, saying multiple modes
    reg_sample <- apply(highest_reg, 2, function(region){
      data_reg <- data[(region["left"] <= data) & (data <= region["right"])]
    })
    n_sampled <- length(reg_sample)
    aggregated_samples <- lapply(1:(n_sampled - 1), function(i, reg_sample){
      tmp <- rbind(
        sample(x = reg_sample[[i]],
               size = ceiling(length(data) / (ncol(highest_reg) - 1)),
               replace = TRUE),
        sample(x = reg_sample[[i + 1]],
               size = ceiling(length(data) / (ncol(highest_reg) - 1)),
               replace = TRUE)
      )
      apply(tmp, 2, aggregation)
    }, reg_sample = reg_sample)
    aggregated_samples
  }
  resampled <- unlist(resampled)[1:length(data)]
  return(resampled)
}
