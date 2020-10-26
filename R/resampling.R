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
          nrow1 <- dim(data)[[1]];
          yy <- rep(c(1,2), c(nrow1, nrow1))
          synth_data <- data.frame(cbind(yy,
                                         rbind(data,
                                               data.frame(g2(data = data)))))
        } else {
          if(strategy == "normal"){#--------------------------
            sample3 <- function(X){
              rnorm(length(X), mean=mean(X), sd = var(X))
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
              data$yy <- sample(
                x = 0:1,
                size = nrow(data),
                replace = TRUE)
              synth_data <- data
            }
          }
        }
      }
    }
  }
  synth_data$yy <- as.factor(synth_data$yy)
  return(synth_data)
}
