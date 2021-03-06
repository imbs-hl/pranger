#' Compute higher density regions for a given vector confidences
#'
#' @param data [vector(1)] Numeric vector containing data. If x is missing then
#'                         den must be provided, and the HDR is computed from
#'                         the given density.
#' @param probs [vector(1)] Probability coverage required for HDRs
#' @param min_region_size [integer(1)] Minimal region size.
#' @param den [density(1)] Density of data as list with components x and y. See
#'                         package hdrcde for more details.
#' @param h [numeric(1)] Optional bandwidth for calculation of density.
#' @param lambda [numeric(1)] Box-Cox transformation parameter where
#'                            0 <= lambda <= 1
#' @param nn [integer(0)] Number of random numbers used in computing f-alpha
#'                        quantiles.
#' @param all.modes [boolean(1)] Return all local modes or just the global mode?
#'
#' @return The highest density region
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- c(rf(500, 10, 90), rnorm(500, 4, 1))
#' probs <- c(50, 75, 95)
#' highestr_res <- highest_regions(data = data,
#'                                 min_region_size = 100,
#'                                 probs = probs)
highest_regions <- function(data,
                            probs = c(95, 75, 50),
                            min_region_size = NULL,
                            den = NULL,
                            h = hdrbw(BoxCox(data, lambda), mean(probs)),
                            lambda = 1,
                            nn = 5000,
                            all.modes = FALSE){
  min_region_size <- if(is.null(min_region_size)){
    sqrt(length(data))
  } else {
    min_region_size
  }
  hr_res_list <- higher_regions(data = data, probs = probs,
                                den = den,
                                h = h,
                                lambda = lambda,
                                nn = nn,
                                all.modes = all.modes)
  region_desc <- hr_res_list$region_desc
  if(max(region_desc$min_size, min_region_size) == min_region_size){
    warning(sprintf("No region has a sample size larger than %s.\n min_region_size has been reset to %s",
                    min_region_size, max(region_desc$min_size)))
    min_region_size <- max(region_desc$min_size)
  }
  region_desc <- region_desc[region_desc$min_size >= min_region_size, ]
  best_index <- which(
    (region_desc$num_grp == max(region_desc$num_grp)) &
      (region_desc$min_size >= max(region_desc$min_size))
  )
  highest_regions <- hr_res_list$regions_list[[best_index]]
  return(highest_regions)
}
