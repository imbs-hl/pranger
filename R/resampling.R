##' Pranger
##'
##' Synthesize a two-classes classification problem given a resampling
##'            strategy.
##'
##' @param data [\code{data.frame}] A \code{data.frame} of original dataset
##' @param strategy [\code{character}] Name of strategy to be used. Most be element of
##'                                "boostrepl", "boostwithoutrepl", "boostbayes",
##'                                "unif", "normal", "binomial" or "boostaggr"
##' @param nb_bootst [\code{integer}] Number of repetitions required to aggregate the
##'                            bootstrap samples. Set to ceiling(sqrt(n)) if not
##'                            provided, with n the number of observations
##' @param aggregation [\code{function}] Function that is called for aggregation. Only
##'                                  usable in the case of "boostaggr"
##'
##' @return [\code{data.frame}] A \code{data.frame} of a synthetic two-classes classification
##'                         problem
##' @export
##'
##' @examples
##'     set.seed(1234)
##'    data_synt <- resampling(
##'                     data = data.frame(x = rnorm(10),
##'                                       y = rnorm(10)),
##'                     strategy = "boostrepl"
##'                   )
##' @import bayesboot
##' @importFrom utils packageVersion
##' @author Cesaire J. K. Fouodo
resampling <- function(
  data,
  strategy,
  nb_bootst,
  aggregation
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
              if(strategy == "boostaggr"){#-----------------------
                ## Define functions
                sample_n_elts <- function(data, nb_bootst){
                  bootstr_sples <- lapply(1:nb_bootst, function(i, data){
                    sample(x = data, replace = TRUE)
                  }, data = data)
                  bootstr_sples <- Reduce(f = "rbind", x = bootstr_sples)
                  if(class(data) %in% c("character", "factor")){
                    bootstr_sples <- if(nb_bootst == 1){
                      c(bootstr_sples)
                    } else {
                      bootstr_sples <- apply(bootstr_sples, 2,
                                             function(i){
                                               sample(i, size = 1,
                                                      replace = FALSE)
                                             })
                    }
                    c(bootstr_sples)
                  } else {
                    if(class(data) %in% c("integer", "numeric")){
                      bootstr_sples <- if(nb_bootst == 1){
                        c(bootstr_sples)
                      } else {
                        bootstr_sples <- apply(bootstr_sples, 2, function(i){
                          aggregation(i)
                        })
                      }
                      # colMeans(bootstr_sples)
                    } else {
                      stop("Unknown variable type for boostaggr resampling strategy...")
                    }
                  }
                  return(bootstr_sples)
                }
                ##
                g4  <- function(data, nb_bootst) {
                  apply(data, 2, function(i, nb_bootst = nb_bootst){
                    sample_n_elts(data = i, nb_bootst)
                  }, nb_bootst = nb_bootst)
                }
                ## Use functions
                nrow1 <- dim(data)[[1]]
                yy <- rep(
                  c(1,2),
                  c(nrow1, nrow1))
                synth_data <- data.frame(
                  cbind(yy, rbind(data, data.frame(g4(data = data,
                                                      nb_bootst = nb_bootst))))
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
