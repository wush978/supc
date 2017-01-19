
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
supc1 <- function(x, tau = 3.5, t = 0.75, tolerance = 1e-4) {
  if (!is.function(t)) {
    parameters <- list(tau = tau, t = function() {t})
  } else {
    parameters <- list(tau = tau, t = t)
  }
  cl.raw <- .supc1.R(x, parameters, tolerance)
  cl <- .clusterize(attr(cl.raw, "dist"), tolerance)
  cl.group <- split(seq_len(nrow(cl.raw)), cl)
  cl.center0 <- lapply(cl.group, function(i) {
    apply(cl.raw[i,,drop = FALSE], 2, mean)
  })
  cl.center <- do.call(rbind, cl.center0)
  retval <- list(cluster = cl, centers = cl.center, size = table(cl))
  class(retval) <- "supc"
  retval
}
