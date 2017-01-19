
.supc1.R <- function(x, parameters, tolerance) {
  repeat{
    d <- .dist(x)
    .T <- parameters$t()
    f <- exp(-d / .T)
    f[d > parameters$tau] <- 0
    f <- as.matrix(f)
    diag(f) <- exp(-0 / .T)
    f <- f / colSums(f)
    .x <- f %*% x
    if (sum(abs(.x - x)) < tolerance) {
      attr(x, "dist") <- d
      break
    }
    x <- .x
  }
  x
}

#'@title Self-Updating Process Clustering
#'
#'@description TODO
#'
#'@param x matrix.
#'@param parameters list.
#'@param implementation either \code{"R"} or \code{"cpp"}.
#'@param tolerance numeric value.
#'
#'
#'
#'@details
#'TODO
#'
#'@return
#'\code{supc1} returns an object of \link{class} "supc".
#'
#'\item{One}{First item}
#'\item{Two}{Second item}
#'
#'@examples
#'print("hello example")
#'@export
supc1 <- function(x, parameters = list(tau = 3.5, t = function() {0.75}), implementation = c("R", "cpp"), tolerance = 1e-4) {
  cl.raw <- switch(
    implementation[1],
    "R" = {
      .supc1.R(x, parameters, tolerance)
    },
    "cpp" = {
      stop("TODO")
    },
    stop("unknown implementation"))
  cl <- .clusterize(attr(cl.raw, "dist"), tolerance)
  cl.group <- split(seq_len(nrow(cl.raw)), cl)
  cl.center0 <- lapply(cl.group, function(i) {
    apply(cl.raw[i,], 2, mean)
  })
  cl.center <- do.call(rbind, cl.center0)
  retval <- list(cluster = cl, centers = cl.center, size = table(cl))
  class(retval) <- "supc"
  retval
}
