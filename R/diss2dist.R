##' Pranger
##'
##' Clean the dissimilarity matrix in input by converting it into a distance
##' matrix
##'
##' @param x [\code{matrix}] A dissimilarity matrix
##'
##' @return [\code{dist}] A distance matrix from class \code{dist}
##' @export
cleandist <- function(x) {
  x <- as.dist(x)
  x[x <= 0] <- 1e-10
  as.matrix(x)
}
