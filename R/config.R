.env <- new.env()

.env$dist.mode <- "stats"
.env$dist.parallelization <- 2

dist.mode <- function(mode = c("stats", "amap", "gputools")) {
  stopifnot(mode[1] %in% c("stats", "amap", "gputools"))
  .env$dist.mode <- mode[1]
  invisible(NULL)
}

dist.parallelization <- function(i) {
  stopifnot(is.integer(i))
  stopifnot(i > 0)
  .env$dist.parallelization <- i
}

.dist <- function(x) {
  switch(.env$dist.mode[1],
    "stat" = stats::dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2),
    "amap" = amap::Dist(x, method = "euclidean", nbproc = .env$dist.parallelization, diag = FALSE, upper = FALSE),
    "gputools" = gputools::gpuDist(x, method = "euclidean", p = 2.0),
    stop("Unknown mode. Please use `dist.mode` to setup the function to compute distance matrix")
  )
}
