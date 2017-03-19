.env <- new.env()

.env$dist.mode <- "stats"
.env$dist.parallelization <- 2L

#'@title Configure which package is used to compute the distance matrix
#'
#'@description
#'Configure which package is used to compute the distance matrix. Note that the fastest implementation depends on the data and machine.
#'
#'@param mode either \code{"stats"}, \code{"amap"}, or \code{"gputools"}.
#'
#'@export
dist.mode <- function(mode = c("stats", "amap", "gputools")) {
  stopifnot(mode[1] %in% c("stats", "amap", "gputools"))
  .env$dist.mode <- mode[1]
  invisible(NULL)
}

#'@title Configure how many cores will be used to calculate the distance matrix
#'
#'@description
#'Only affect \code{\link[amap]{Dist}}.
#'
#'@param i integer.
#'@export
dist.parallelization <- function(i) {
  if (is.numeric(i)) {
    if (as.integer(i) == i) i <- as.integer(i) else stop("i must be an integer")
  }
  stopifnot(is.integer(i))
  stopifnot(i > 0)
  .env$dist.parallelization <- i
}

.dist <- function(x) {
  switch(.env$dist.mode[1],
    "stats" = stats::dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2),
    "amap" = amap::Dist(x, method = "euclidean", nbproc = .env$dist.parallelization, diag = FALSE, upper = FALSE),
    "gputools" = gputools::gpuDist(x, method = "euclidean", p = 2.0),
    stop("Unknown mode. Please use `dist.mode` to setup the function to compute distance matrix")
  )
}

#'@importFrom Rcpp sourceCpp
#'@useDynLib supc, .registration = TRUE 
.onLoad <- function(libname, pkgname) { }
