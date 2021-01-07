#' resampling builts a two-classes classification problem using a resampling
#'            strategy. This implementation is comparable to
#'            that of Shi and Hovarth (2006)
#'
#' @param data [data.frame(1)] A \code{data.frame} of original dataset
#' @param strategy [character(1)] Name of strategy to be used. Most be element of
#'                                "boostrepl", "boostwithoutrepl", "boostbayes",
#'                                "unif", "normal" or "binomial"
#'
#' @return [data.frame(1)] A \code{data.frame} of a two-classes classification
#'                         problem
#' @export
#'
#' @examples
#'     set.seed(1234)
#'    data_synt <- resampling(
#'                     data = data.frame(x = rnorm(10),
#'                                       y = rnorm(10)),
#'                     strategy = "boostrepl"
#'                   )
#' @import bayesboot
#' @author Cesaire J. K. Fouodo
#' @import gtools
#' @importFrom utils packageVersion
resampling <- function(
  data,
  strategy
){
  synth_data <- NULL
  sample_boostrepl <- function(tmp_data){
    sample(tmp_data, replace = TRUE)
  }
  sample_boostwithoutrepl <- function(tmp_data){
    sample(tmp_data, replace = FALSE)
  }
  g_boostrepl <- function(tmp_data){
    apply(tmp_data, 2, sample_boostrepl)
  }
  g_boostwithoutrepl <- function(tmp_data){
    apply(tmp_data, 2, sample_boostwithoutrepl)
  }
  if(strategy == "boostrepl"){#--------------------------
    nrow1 <- dim(data)[[1]]
    yy <- rep(c(1,2), c(nrow1, nrow1))
    synth_data <- data.frame(cbind(yy,
                                   rbind(data,
                                         data.frame(
                                           g_boostrepl(tmp_data = data))))
                             )
  } else {
    if(strategy == "boostwithoutrepl"){#--------------------------
      nrow1 <- dim(data)[[1]]
      yy <- rep(c(1, 2), c(nrow1, nrow1) )
      synth_data <- data.frame(cbind(yy, rbind(data,
                                               data.frame(
                                                 g_boostwithoutrepl(
                                                   tmp_data = data))))
                               )
    } else {
      if(strategy == "boostbayes"){#--------------------------
        drawn <- function(x){
          rnorm(n = 1, mean = mean(x), sd = sd(x))
        }
        sample_bayes <- function(a){
          tmp <- bayesboot::bayesboot(data = a,
                                      statistic = drawn,
                                      use.weights = FALSE)$V1
          sample(tmp, length(a), replace = FALSE)
        }
        g1 <-  apply(data, 2, sample_bayes)
        nrow1 <- dim(data)[[1]]
        yy <- rep(c(1,2), c(nrow1, nrow1))
        synth_data <- data.frame(cbind(yy, rbind(data, g1)))
      } else {
        if(strategy == "unif"){#--------------------------
          sample2 <- function(X){
            runif(length(X), min=min(X), max =max(X)) }
          g2  <- function(data){
            apply(data,2,sample2)}
          nrow1 <- dim(data)[[1]]
          yy <- rep(c(1,2), c(nrow1, nrow1))
          synth_data <- data.frame(cbind(yy,
                                         rbind(data,
                                               data.frame(g2(data = data)))))
        } else {
          if(strategy == "normal"){#--------------------------
            sample3 <- function(X){
              rnorm(length(X), mean=mean(X), sd = sd(X))
            }
            g3  <- function(data) {
              apply(data,2,sample3)
            }
            nrow1 <- dim(data)[[1]]
            yy <- rep(
              c(1,2),
              c(nrow1, nrow1))
            synth_data <- data.frame(
              cbind(yy, rbind(data, data.frame(g3(data = data))))
            )
          } else {
            if(strategy == "binomial"){#--------------------------
              yy <- sample(
                x = 0:1,
                size = nrow(data),
                replace = TRUE)
              synth_data <- data.frame(cbind(yy, data))
            } else {
              if(strategy == "rejection"){#-----------------------
                ## Define functions
                sample_one_elt <- function(data = data, delta) {
                  width <- max(data) - min(data)
                  min_data <- min(data)
                  ## Normalization
                  data <- (data - min_data) / width
                  ## We assume x to have values between 0 and 1
                  accepted <- FALSE
                  data <- sort(data, decreasing = FALSE)
                  delta <- if(missing(delta)){
                    10 * (1 / (length(data)))
                  } else {
                    delta
                  }
                  n_intervall <- ceiling((max(data) - min(data)) / delta) - 1
                  intervall_frq <- lapply(0:n_intervall,
                                          function(intervall, data){
                                            frq <- sum(
                                              (intervall*delta <= data) & (data < (intervall+1)*delta)
                                            )
                                            return(frq)
                                          }, data = data)

                  intervall_frq <- unlist(intervall_frq)
                  ## Empirical probabilities
                  x <- NULL
                  while(!accepted) {
                    ## Draw interval according to probability to be accepted
                    draw_frq <- unlist(mapply(rep, 0:n_intervall, intervall_frq * 1/delta))
                    x_unif <- runif(n = length(draw_frq), min = 0, max = 1)
                    within_interval <- ((draw_frq*delta <= x_unif) &
                                          (x_unif < (draw_frq+1)*delta))
                    accepted <- any(unlist(within_interval))
                    x <- if(accepted){
                      sample(x = x_unif[unlist(within_interval)], size = 1)
                    }
                  }
                  x <- x * width + min_data
                  return(x)
                }
                # sample_one_elt <- function(data = data, delta) {
                #   width <- max(data) - min(data)
                #   min_data <- min(data)
                #   ## Normalization
                #   data <- (data - min_data) / width
                #   ## We assume x to have values between 0 and 1
                #   n <- length(data)
                #   accepted <- FALSE
                #   data <- sort(data, decreasing = FALSE)
                #   delta <- if(missing(delta)){
                #     1 / (length(data))
                #   } else {
                #     delta
                #   }
                #   ecdf_data <- ecdf(data)
                #   prob_intervall <- lapply(0:(n-1), function(intervall, ecdf_data, data){
                #     prob <- diff(ecdf_data(c(intervall*delta, (intervall+1)*delta)))
                #     return(prob)
                #   }, ecdf_data = ecdf_data, data = data)
                #   prob_intervall <- unlist(prob_intervall)
                #
                #   ## Empirical probabilities
                #   x <- NULL
                #   while(!accepted) {
                #     intervall <- sample(
                #       x = 0:(n-1),
                #       size = 1,
                #       prob = prob_intervall,
                #       replace = FALSE
                #     )
                #     x <- runif(n = 1, min = 0, max = 1)
                #     accepted <- ((intervall*delta <= x) & (x < (intervall+1)*delta))
                #   }
                #   x <- x * width + min_data
                #   return(x)
                # }
                sample_n_elts <- function(n, data = data, delta){
                  delta <- if(missing(delta)){
                    10(1 / (length(data)))
                  } else {
                    delta
                  }
                  my_sample <- unlist(
                    lapply(1:length(data), function(i, data, delta = delta){
                      sample_one_elt(data = data, delta = delta)
                    },
                    data = data, delta = delta))
                  return(my_sample)
                }
                ## Use functions
                g3  <- function(data) {
                  ##!! ToDo: Take delta into account in rejection sampling
                  ##!! Now it set as default
                  apply(data, 2, function(i){
                    sample_n_elts(n = length(i), data = i)
                  })
                }
                nrow1 <- dim(data)[[1]]
                yy <- rep(
                  c(1,2),
                  c(nrow1, nrow1))
                synth_data <- data.frame(
                  cbind(yy, rbind(data, data.frame(g3(data = data))))
                )
              }
            }
          }
        }
      }
    }
  }
  synth_data$yy <- as.factor(synth_data$yy)
  return(synth_data)
}
