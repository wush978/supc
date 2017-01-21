
.supc1.R <- function(x, parameters, tolerance) {
  stopifnot(length(parameters$tau) == length(parameters$t))
  lapply(seq_along(parameters$tau), function(i) {
    .current.tau <- parameters$tau[i]
    .current.t <- parameters$t[[i]]
    is.first <- TRUE
    t <- 0
    repeat{
      if (is.first) {
        if (!is.null(parameters$d0)) {
          d <- parameters$d0
        } else {
          d <- .dist(x)
          parameters$d0 <- d
        }
      } else d <- .dist(x)
      .T <- .current.t(t <- t + 1)
      f <- exp(-d / .T)
      f[d > .current.tau] <- 0
      f <- as.matrix(f)
      diag(f) <- exp(-0 / .T)
      f <- f / colSums(f)
      .x <- f %*% x
      if (sum(abs(.x - x)) < tolerance) {
        attr(x, "dist") <- d
        break
      }
      x <- .x
      is.first <- FALSE
    }
    x
  })
}

.get.parameters <- function(x, r, rp, t) {
  retval <- list()
  # tau
  if (is.null(r)) {
    d0 <- .dist(x)
    retval$tau <- as.vector(quantile(d0, rp))
    retval$d0 <- d0
  } else {
    retval$tau <- r
  }
  # t
  if (is.numeric(t)) {
    if (length(t) != length(retval$tau)) stop("The length of r, rp and t are inconsistent")
    retval$t <- lapply(force(t), function(.t) {
      .t <- force(.t)
      function(t) {.t}      
    })
  } else if (is.function(t)) {
    retval$t <- rep(list(t), length(retval$tau))
  } else if (is.character(t)) {
    retval$t <- switch(t,
                       "static" = lapply(retval$tau, function(r) {
                         force(r)
                         function(t) {r / 5}
                         }),
                       "dynamic" = lapply(retval$tau, function(r) {
                         force(r)
                         function(t) {r / 20 + t * (r / 50)}
                       }),
                       stop("Invalid parameter t")
    )
  } else if (is.list(t)) {
    if (length(t) != length(retval$tau)) stop("The length of r, rp and t are inconsistent")
    lapply(t, function(.t) {
      if (!is.function(.t)) stop("Invalid parameter t")
    })
    retval$t <- t
  } else {
    stop("Invalid parameter t")
  }
  retval
}

#'@title Self-Updating Process Clustering
#'
#'@description TODO
#'
#'@param x matrix. Each row is an instance of the data.
#'@param r numeric vector or \code{NULL}. A parameter of the self-updating process. Please see section 2.3.1 of the paper Shiu and Chen 2016.
#'@param rp numeric vector or \code{NULL}. If \code{r} is \code{NULL}, then \code{rp} will be used. The corresponding \code{r} is the \code{rp}-percentiles of the pairwise distances of the data. If both of \code{r} and \code{rp} are \code{NULL}, then the default value is \code{rp = c(0.005, 0.01, 0.03, 0.1)}.
#'@param t either numeric value, function, or one of \code{"static" or "dynamic"}.
#'@param tolerance numeric value. The threshold of convergence.
#'@param drop logical value. Whether to delete the list structure if its length is 1.
#'
#'
#'
#'@details
#'TODO
#'
#'@return
#'\code{supc1} returns a list of objects of \link{class} "supc".
#'
#'Each "supc" object contains the following elements:
#'\item{cluster}{The cluster id of each instances.}
#'\item{centers}{The center of each cluster.}
#'\item{size}{The size of each cluster.}
#'
#'@examples
#'print("hello example")
#'
#'@references
#'Shiu, Shang-Ying, and Ting-Li Chen. 2016. “On the Strengths of the Self-Updating Process Clustering Algorithm.” Journal of Statistical Computation and Simulation 86 (5): 1010–1031. doi:10.1080/00949655.2015.1049605. \url{http://dx.doi.org/10.1080/00949655.2015.1049605}.
#'@export
supc1 <- function(x, r = NULL, rp = NULL, t = c("static", "dynamic"), tolerance = 1e-4, drop = TRUE) {
  parameters <- .get.parameters(x, r, rp, t)
  cl.raw <- .supc1.R(x, parameters, tolerance)
  retval <- lapply(
    cl.raw,
    function(.raw) {
      cl <- .clusterize(attr(.raw, "dist"), tolerance)
      cl.group <- split(seq_len(nrow(.raw)), cl)
      cl.center0 <- lapply(cl.group, function(i) {
        apply(.raw[i,,drop = FALSE], 2, mean)
      })
      cl.center <- do.call(rbind, cl.center0)
      retval <- list(cluster = cl, centers = cl.center, size = table(cl))
      class(retval) <- "supc"
      retval
    })
  if (drop && length(retval) == 1) retval[[1]] else retval
}
