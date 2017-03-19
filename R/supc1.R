
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
#'@description 
#'The SUP is a distance-based method for clustering. 
#'The idea of this algorithm is similar to gravitational attraction: every sample gravitates towards one another. 
#'The algorithm mimics the process of gravitational attraction iteratively that eventually merges the samples into clusters on the sample space. 
#'During the iterations, all samples continue moving until the system becomes stable.
#'
#'@param x matrix. Each row is an instance of the data.
#'@param r numeric vector or \code{NULL}. The parameter \eqn{r} of the self-updating process.
#'@param rp numeric vector or \code{NULL}. If \code{r} is \code{NULL}, then \code{rp} will be used. 
#'The corresponding \code{r} is the \code{rp}-percentile of the pairwise distances of the data. 
#'If both \code{r} and \code{rp} are \code{NULL}, then the default value is \code{rp = c(0.0005, 0.001, 0.01, 0.1, 0.3)}.
#'@param t either numeric vector, list of function, or one of \code{"static" or "dynamic"}. The parameter \eqn{T(t)} of the self-updating process.
#'@param tolerance numeric value. The threshold of convergence.
#'@param drop logical value. Whether to delete the list structure if its length is 1.
#'@param implementation eithor \code{"R"} or \code{"cpp"}. Choose the tool to calculate result.
#'@param verbose logical value. Whether to show the iteration history.
#'
#'@details
#'Please check the vignettes via \code{vignettes("supc", package = "supc")} for details.
#'
#'@return
#'\code{supc1} returns a list of objects of \link{class} "supc".
#'
#'Each "supc" object contains the following elements:
#'\item{x}{The input matrix.}
#'\item{d0}{The pairwise distance matrix of \code{x}.}
#'\item{r}{The value of \eqn{r} of the clustering.}
#'\item{t}{The function \eqn{T(t)} of the clustering.}
#'\item{cluster}{The cluster id of each instance.}
#'\item{centers}{The center of each cluster.}
#'\item{size}{The size of each cluster.}
#'
#'@examples
#'\dontrun{
#'set.seed(1)
#'X <- local({
#'  mu <- list(
#'    x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
#'    y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
#'  )
#'  X <- lapply(1:5, function(i) {
#'    cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
#'  })
#'  X <- do.call(rbind, X)
#'  n <- nrow(X)
#'  X <- rbind(X, matrix(0, 20, 2))
#'  k <- 1
#'  while(k <= 20) {
#'    tmp <- c(13*runif(1)-2.5, 8*runif(1)-2.5)
#'    y1 <- mu$x - tmp[1]
#'    y2 <- mu$y - tmp[2]
#'    y <- sqrt(y1^2+y2^2)
#'    if (min(y)> 2){
#'      X[k+n,] <- tmp
#'      k <- k+1
#'    }
#'  }
#'  X
#'})
#'X.supcs <- supc1(X, r = c(0.9, 1.7, 2.5), t = "dynamic")
#'X.supcs$cluster
#'plot(X.supcs[[1]], type = "heatmap", major.size = 2)
#'plot(X.supcs[[2]], type = "heatmap", col = cm.colors(24), major.size = 5)
#'
#'X.supcs <- supc1(X, r = c(1.7, 2.5), t = list(
#'  function(t) {1.7 / 20 + exp(t) * (1.7 / 50)},
#'  function(t) {exp(t)}
#'))
#'plot(X.supcs[[1]], type = "heatmap", major.size = 2)
#'plot(X.supcs[[2]], type = "heatmap", col = cm.colors(24), major.size = 5)
#'}
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
#'  \item{\code{"heatmap"}}{draw a heatmap to show the result of clustering. The clusters whose size is greater than parameter \code{major.size} are treated as major clusters.}
#'}
#'@param ... other parameters to be passed through.
#'
#'@examples
#'\dontrun{
#'data(golub, package = "supc")
#'golub.supc <- supc1(golub, rp = 0.0005, t = "dynamic")
#'table(golub.supc$size)
#'plot(golub.supc, type = "heatmap", major.size = 10)
#'}
#'
#'@export
plot.supc <- function(x, type = "heatmap", ...) {
  switch(type, "heatmap" = heatmap.supc(x, ...), stop("unsupported type"))
}

heatmap.supc <- function(x, ..., major.size = 1, yaxt = "n", xlab = "Samples", ylab = "Variables", mgp = c(1.5, 0, 0), title.digits = 4, display.minor.size = FALSE) {
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush())
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  argv <- list(..., x = seq_len(nrow(x$x)), y = seq_len(ncol(x$x)), z = x$x[order(x$cluster),], yaxt = yaxt, xlab = xlab, ylab = ylab, mgp = mgp)
  do.call(graphics::image, argv)
  title(sprintf("r=%s", format(x$r, digits = title.digits)), line = 2.5)
  x.at.tail <- cumsum(x$size)
  x.at.head <- c(0, utils::head(x.at.tail, -1))
  graphics::abline(v = x.at.tail[x$size > major.size] + 0.5, lty = 2)
  graphics::axis(side=2, at=seq_len(ncol(x$x)), labels=dimnames(x$x)[[2]], tick=FALSE, mgp=c(1.5,0,0))
  major.at <- apply(cbind(x.at.head[x$size > major.size], x.at.tail[x$size > major.size]), 1, mean) + 0.5
  major.label <- x$size[x$size > major.size]
  graphics::mtext("Cluster Size", side = 3, line=1, padj = -0.5)
  minor.size.table <- table(x$size[x$size <= major.size])
  minor.size <- sort(unique(x$size[x$size <= major.size]), decreasing = TRUE)
  minor.at <- numeric(length(minor.size))
  segment.height <- graphics::par()$usr[4] + 0.1 * (diff(graphics::par()$usr[3:4]) / diff(graphics::par()$plt[3:4]) * (1 - graphics::par()$plt[4]))
  y0 <- graphics::par()$usr[4]
  y1 <- segment.height
  if (display.minor.size) {
    for(i in seq_along(minor.size)) {
      size <- minor.size[i]
      x0 <- min(x.at.head[x$size == size]) + 0.6
      x1 <- max(x.at.tail[x$size == size]) + 0.4
      graphics::segments(y0 = y1, x0 = x0, x1 = x1, xpd = TRUE)
      graphics::segments(y0 = y0, x0 = x0, y1 = y1, xpd = TRUE)
      graphics::segments(y0 = y0, x0 = x1, y1 = y1, xpd = TRUE)
      minor.at[i] <- mean(c(x0, x1))
    }
  }
  graphics::axis(side = 3, at = c(major.at, minor.at), labels = c(major.label, minor.size), tick = FALSE, mgp = c(1.5, 0, 0), padj = -0.5)
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