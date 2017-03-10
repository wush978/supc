
.supc1.cpp <- function(x, parameters, tolerance, verbose) {
  stopifnot(length(parameters$tau) == length(parameters$t))
  lapply(seq_along(parameters$tau), function(i) {
    .current.tau <- parameters$tau[i]
    .current.t <- parameters$t[[i]]
    .supc1.cpp.internal(x, .current.tau, .current.t, tolerance, .dist, verbose)
  })
}

.supc1.R <- function(x, parameters, tolerance, verbose) {
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
      .T <- .current.t(t)
      t <- t + 1
      f <- exp(-d / .T)
      f[d > .current.tau] <- 0
      f <- as.matrix(f)
      diag(f) <- exp(-0 / .T)
      f <- f / colSums(f)
      .x <- f %*% x
      .difference <- max(abs(.x - x))
      if (verbose) cat(sprintf("difference: %0.8f\n", .difference))
      if (.difference < tolerance) {
        attr(x, "dist") <- d
        attr(x, "iteration") <- t
        break
      }
      x <- .x
      is.first <- FALSE
    }
    x
  })
}

.rp.default <- c(0.0005, 0.001, 0.01, 0.1, 0.3)
.t.static <- function(r) {
  function(t) {r / 5}
}
.t.dynamic <- function(r) {
  function(t) {r / 20 + t * (r / 50)}
}

#'@importFrom stats quantile heatmap
.get.parameters <- function(x, r, rp, t) {
  d0 <- .dist(x)
  retval <- list(d0 = d0)
  # tau
  if (is.null(r)) {
    if (is.null(rp)) {
      retval$tau <- as.vector(quantile(d0, .rp.default))
    } else {
      retval$tau <- as.vector(quantile(d0, rp))
    }
  } else {
    retval$tau <- r
  }
  # t
  if (is.numeric(t)) {
    if (length(t) == 1) {
      retval$t <- lapply(seq_along(retval$tau), function(.i) {
        .t <- force(t)
        function(t) {.t}
      })
    } else if (length(t) == length(retval$tau)) {
      retval$t <- lapply(force(t), function(.t) {
        .t <- force(.t)
        function(t) {.t}      
      })
    } else stop("The length of r, rp and t are inconsistent")
  } else if (is.function(t)) {
    retval$t <- rep(list(t), length(retval$tau))
  } else if (is.character(t)) {
    retval$t <- switch(t[1],
                       "static" = lapply(retval$tau, function(r) {
                         force(r)
                         .t.static(r)
                         }),
                       "dynamic" = lapply(retval$tau, function(r) {
                         force(r)
                         .t.dynamic(r)
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
#'@param r numeric vector or \code{NULL}. The parameter \eqn{r} of the self-updating process.
#'@param rp numeric vector or \code{NULL}. If \code{r} is \code{NULL}, then \code{rp} will be used. 
#'The corresponding \code{r} is the \code{rp}-percentiles of the pairwise distances of the data. 
#'If both \code{r} and \code{rp} are \code{NULL}, then the default value is \code{rp = c(0.0005, 0.001, 0.01, 0.1, 0.3)}.
#'@param t either numeric value, function, or one of \code{"static" or "dynamic"}. The parameter \eqn{T(t)} of the self-updating process.
#'@param tolerance numeric value. The threshold of convergence.
#'@param drop logical value. Whether to delete the list structure if its length is 1.
#'@param implementation eithor \code{"R"} or \code{"cpp"}. Choose the tool to calculating result.
#'@param verbose logical value. Whether to show some messages during computing or not.
#'
#'@details
#'TODO
#'
#'@return
#'\code{supc1} returns a list of objects of \link{class} "supc".
#'
#'Each "supc" object contains the following elements:
#'\item{x}{The input matrix.}
#'\item{d0}{The pairwise distance matrix of \code{x}.}
#'\item{r}{The value of \eqn{r} of the clustering.}
#'\item{t}{The function \eqn{T(t)} of the clustering.}
#'\item{cluster}{The cluster id of each instances.}
#'\item{centers}{The center of each cluster.}
#'\item{size}{The size of each cluster.}
#'
#'@examples
#'print("hello example")
#'
#'@references
#'Shiu, Shang-Ying, and Ting-Li Chen. 2016. "On the Strengths of the Self-Updating Process Clustering Algorithm." Journal of Statistical Computation and Simulation 86 (5): 1010–1031. doi:10.1080/00949655.2015.1049605. \url{http://dx.doi.org/10.1080/00949655.2015.1049605}.
#'@export
supc1 <- function(x, r = NULL, rp = NULL, t = c("static", "dynamic"), tolerance = 1e-4, drop = TRUE, implementation = c("cpp", "R"), verbose = FALSE) {
  parameters <- .get.parameters(x, r, rp, t)
  cl.raw <- switch(implementation[1], "R" = .supc1.R(x, parameters, tolerance, verbose), "cpp" = .supc1.cpp(x, parameters, tolerance, verbose))
  retval <- lapply(
    seq_along(cl.raw),
    function(.i) {
      .raw <- cl.raw[[.i]]
      cl <- .clusterize(attr(.raw, "dist"), tolerance)
      cl.group <- split(seq_len(nrow(.raw)), cl)
      cl.center0 <- lapply(cl.group, function(i) {
        apply(.raw[i,,drop = FALSE], 2, mean)
      })
      cl.center <- do.call(rbind, cl.center0)
      retval <- list(x = x, d0 = parameters$d0, r = as.vector(parameters$tau[.i]), t = parameters$t[[.i]], cluster = cl, centers = cl.center, size = table(cl))
      class(retval) <- "supc"
      attr(retval, "iteration") <- attr(.raw, "iteration")
      retval
    })
  if (drop && length(retval) == 1) retval[[1]] else {
    class(retval) <- "supclist"
    retval
  }
}

#'@title Plot the frequency polygon of pairwise distance
#'
#'@param x either dist object or matrix.
#'@param ... other parameters to be passed through to \code{\link[graphics]{hist}}.
#'
#'@description
#'Plot the frequency polygon of the pairwise distance.
#'@aliases freq.poly.default freq.poly.dist
#'@export
freq.poly <- function(x, ...) {
  UseMethod("freq.poly")
}

#'@importFrom graphics hist plot lines title
#'@export
freq.poly.default <- function(x, ...) {
  d <- .dist(as.matrix(x))
  .hist <- hist(as.vector(d), plot = FALSE, ...)
  plot(.hist$mids, .hist$counts, type = "l", xlab = "Distance", ylab = "Frequency", main = "Frequency Polygon of Pairwise Distance")
  invisible(.hist)
}

#'@export
freq.poly.dist <- function(x, ...) {
    d <- x
  .hist <- hist(as.vector(d), plot = FALSE, ...)
  plot(.hist$mids, .hist$counts, type = "l", xlab = "Distance", ylab = "Frequency", main = "Frequency Polygon of Pairwise Distance")
  invisible(.hist)
}

#'@title Plot the frequency polygon of pairwise distance
#'
#'@param x either dist object or matrix.
#'@param ... other parameters to be passed through to \code{\link[graphics]{hist}}.
#'
#'@description
#'Plot the frequency polygon of the pairwise distance. The red dashed line is the used parameter \eqn{r}.
#'@aliases freq.poly.subclist
#'@export
freq.poly.supc <- function(x, ...) {
  .hist <- freq.poly(x$d0, ...)
  lines(list(x = rep(x$r, 2), y = range(.hist$counts)), col = 2, lty = 2)
  title(sub = sprintf("r = %f", x$r))
}

#'@export
freq.poly.supclist <- function(x, ...) {
  .hist <- freq.poly(x[[1]]$d0, ...)
  lapply(x, function(x) lines(list(x = rep(x$r, 2), y = range(.hist$counts)), col = 2, lty = 2))
  title(sub = sprintf("r = %s", deparse(sapply(x, "[[", "r"))))
  .hist
}

#'@export
`$.supclist` <- function(obj, name) {
  retval <- lapply(obj, "[[", name)
  names(retval) <- sprintf("r=%f", lapply(obj, "[[", "r"))
  retval
}

#'@title Draw plots of the clustering result
#'
#'@description
#'
#'General function to draw plots for analysis
#'
#'@param x \code{supc} object to plot.
#'@param type character value. \itemize{
#'  \item{\code{"heatmap"}}{draw a heatmap to show the result of clustering}
#'}
#'@param ... other parameters to be passed through.
#'
#'@export
plot.supc <- function(x, type = "heatmap", ...) {
  switch(type, "heatmap" = heatmap.supc(x, ...), stop("unsupported type"))
}

heatmap.supc <- function(x, ..., yaxt = "n", xlab = "Samples", ylab = "Variables", mgp = c(1.5, 0, 0)) {
  argv <- list(..., x = seq_len(nrow(x$x)), y = seq_len(ncol(x$x)), z = x$x[order(x$cluster),], yaxt = yaxt, xlab = xlab, ylab = ylab, mgp = mgp)
  do.call(image, argv)
  title(paste0("r=", x$r), line = 2.5)
  abline(v = cumsum(x$size) + 0.5, lty = 2)
  axis(side=2, at=seq_len(ncol(x$x)), labels=dimnames(x$x)[[2]], tick=FALSE, mgp=c(1.5,0,0))
  axis(side=3, at=cumsum(x$size) + 0.5, labels=x$size, tick=FALSE, mgp=c(1.5,0,0))
  mtext("Cluster Size", side = 3, line=1, cex=0.8)
}

#'@name golub
#'@title Gene expression dataset from Golub et al. (1999)
#'@description
#'Gene expression data (3051 genes and 38 tumor mRNA samples) from the leukemia microarray study of Golub et al. (1999). 
#'Each row (gene) is scaled to mean 0 and standard deviation 1.
#'
#'@return
#'\item{golub}{The matrix of scaled gene expression data.}
#'\item{golub.supc}{The result of \code{golub.supc <- supc1(golub, r = c(4, 4.3, 4.6, 4.7, 4.8), t = "dynamic")}}
#'
#'@aliases golub.supc
NULL