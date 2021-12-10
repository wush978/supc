.env <- new.env()

.env$dist.mode <- "stats"
.env$dist.parallelization <- 2L
.env$dist.FUN <- list(
  "stats" = function(x) stats::dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2),
  "amap" = function(x) amap::Dist(x, method = "euclidean", nbproc = .env$dist.parallelization, diag = FALSE, upper = FALSE)
)

#'@title Configure which package is used to compute the distance matrix
#'
#'@description
#'Configure which package is used to compute the distance matrix or register one.
#'Note that the speed depends on the data and the hardware.
#'
#'@param mode string. The available modes are \code{"stats"} and \code{"amap"} by default.
#'@param FUN a function which has one argument \code{x} or \code{NULL}. 
#'The function should compute the pairwise distance of \code{x} and return a \code{dist} object.
#'The user can skip this argument if the \code{mode} is registered. For example, \code{"stats"}
#'and \code{"amap"} are registered by default.
#'@return \code{NULL}. The function is called for side effects.
#'@examples
#'# use stats::dist to compute the pairwise distance
#'dist.mode("stats")
#'# use amap::Dist to compute the pairwise distance
#'dist.mode("amap")
#'@export
dist.mode <- function(mode = c("stats", "amap"), FUN = NULL) {
  if (is.null(FUN)) {
    stopifnot(mode[1] %in% names(.env$dist.FUN))
  } else {
    .env$dist.FUN[[mode[1]]] <- FUN
  }
  .env$dist.mode <- mode[1]
  invisible(NULL)
}

#'@title Configure how many cores will be used to calculate the distance matrix
#'
#'@description
#'Only affect \code{\link[amap]{Dist}}.
#'
#'@param i integer.
#'@return \code{NULL}. The function is called for side effects.
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
  if (!.env$dist.mode[1] %in% names(.env$dist.FUN)) stop("Unknown mode. Please use `dist.mode` to setup the function to compute distance matrix")
  .env$dist.FUN[[.env$dist.mode[1]]](x)
}

#'@importFrom Rcpp sourceCpp
#'@useDynLib supc, .registration = TRUE 
.onLoad <- function(libname, pkgname) { }
