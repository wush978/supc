
.supc1.cpp2 <- function(x, parameters, tolerance, verbose) {
  stopifnot(length(parameters$tau) == length(parameters$t))
  lapply(seq_along(parameters$tau), function(i) {
    .current.tau <- parameters$tau[i]
    .current.t <- parameters$t[[i]]
    .supc1.cpp2.internal(x, .current.tau, .current.t, tolerance, verbose)
  })
}

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
  retval <- list()
  # tau
  if (is.null(r)) {
    d0 <- .dist(x)
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
#'@param x data matrix. Each row is an instance of the data.
#'@param r numeric vector or \code{NULL}. The parameter \eqn{r} of the self-updating process.
#'@param rp numeric vector or \code{NULL}. If \code{r} is \code{NULL}, then \code{rp} will be used. 
#'The corresponding \code{r} is the \code{rp}-percentile of the pairwise distances of the data. 
#'If both \code{r} and \code{rp} are \code{NULL}, then the default value is \code{rp = c(0.0005, 0.001, 0.01, 0.1, 0.3)}.
#'@param t either numeric vector, list of function, or one of \code{"static" or "dynamic"}. The parameter \eqn{T(t)} of the self-updating process.
#'@param tolerance numeric value. The threshold of convergence.
#'@param cluster.tolerance numeric value. After iterations, if the distance of two points are smaller than \code{cluster.tolerance},
#'then they are identified as in the same cluster.
#'@param drop logical value. Whether to delete the list structure if its length is 1.
#'@param implementation eithor \code{"R"}, \code{"cpp"} or \code{"cpp2"}. Choose the engine to calculate result.
#'The \code{"cpp2"} parallelly computes the distance in C++ with OpenMP, which is not supported under OS X, and uses the early-stop to speed up calculation.
#'@param sort logical value. Whether to sort the cluster id by size.
#'@param verbose logical value. Whether to show the iteration history.
#'
#'@details
#'Please check the vignettes via \code{vignette("supc", package = "supc")} for details.
#'
#'@return
#'\code{supc1} returns a list of objects of \link{class} "supc".
#'
#'Each "supc" object contains the following elements:
#'\item{x}{The input matrix.}
#'\item{d0}{The pairwise distance matrix of \code{x} or \code{NULL}.}
#'\item{r}{The value of \eqn{r} of the clustering.}
#'\item{t}{The function \eqn{T(t)} of the clustering.}
#'\item{cluster}{The cluster id of each instance.}
#'\item{centers}{The center of each cluster.}
#'\item{size}{The size of each cluster.}
#'\item{iteration}{The number of iterations before convergence.}
#'\item{result}{The position of data after iterations.}
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
supc1 <- function(
  x, 
  r = NULL, 
  rp = NULL, 
  t = c("static", "dynamic"), 
  tolerance = 1e-4, 
  cluster.tolerance = 10 * tolerance, 
  drop = TRUE, 
  implementation = c("cpp", "R", "cpp2"), 
  sort = TRUE,
  verbose = (nrow(x) > 10000)
  ) {
  parameters <- .get.parameters(x, r, rp, t)
  cl.raw <- switch(
    implementation[1], 
    "R" = .supc1.R(x, parameters, tolerance, verbose), 
    "cpp" = .supc1.cpp(x, parameters, tolerance, verbose),
    "cpp2" = .supc1.cpp2(x, parameters, tolerance, verbose)
    )
  retval <- lapply(
    seq_along(cl.raw),
    function(.i) {
      .raw <- cl.raw[[.i]]
      cl <- .clusterize(.raw, cluster.tolerance)
      cl.size <- table(cl)
      if (sort) {
        .rank <- rank(-cl.size, ties.method = "first")
        cl <- .rank[cl]
        names(cl) <- NULL
        cl.size <- table(cl) 
      }
      cl.group <- split(seq_len(nrow(.raw)), cl)
      cl.center0 <- lapply(cl.group, function(i) {
        apply(.raw[i,,drop = FALSE], 2, mean)
      })
      cl.center <- do.call(rbind, cl.center0)
      retval <- list(
        x = x, 
        d0 = parameters$d0, 
        r = as.vector(parameters$tau[.i]), 
        t = parameters$t[[.i]], 
        cluster = cl, 
        centers = cl.center, 
        size = cl.size,
        result = structure(as.vector(.raw), .Dim = dim(.raw)),
        iteration = attr(.raw, "iteration")
      )
      class(retval) <- "supc"
      retval
    })
  if (drop && length(retval) == 1) retval[[1]] else {
    class(retval) <- "supclist"
    retval
  }
}

#'@title Randomized Self-Updating Process Clustering
#'
#'@description 
#'The Randomized Self-Updating Process Clustering (randomized SUP) is a modification of the original SUP algorithm. 
#'The randomized SUP randomly generates the partition of the instances during each iterations. 
#'At each iteration, the self updating process is conducted independently in each partition in order to reduce the computation and the memory.
#'
#'@param x data matrix. Each row is an instance of the data.
#'@param r numeric vector or \code{NULL}. The parameter \eqn{r} of the self-updating process.
#'@param rp numeric vector or \code{NULL}. If \code{r} is \code{NULL}, then \code{rp} will be used. 
#'The corresponding \code{r} is the \code{rp}-percentile of the pairwise distances of the data. 
#'If both \code{r} and \code{rp} are \code{NULL}, then the default value is \code{rp = c(0.0005, 0.001, 0.01, 0.1, 0.3)}.
#'@param t either numeric vector, list of function, or one of \code{"static" or "dynamic"}. The parameter \eqn{T(t)} of the self-updating process.
#'@param k integer value. The number of the partitions.
#'@param groups list. The first element is the partition of the first iteration, and the second element is the partition
#'of the second iteration, etc. If the number of the iteration exceeds \code{length(groups)}, then new partition will be 
#'generated.
#'@param tolerance numeric value. The threshold of convergence.
#'@param cluster.tolerance numeric value. After iterations, if the distance of two points are smaller than \code{cluster.tolerance},
#'then they are identified as in the same cluster.
#'@param drop logical value. Whether to delete the list structure if its length is 1.
#'@param implementation eithor \code{"R"} or \code{"cpp"}. Choose the engine to calculate result.
#'@param sort logical value. Whether to sort the cluster id by size.
#'@param verbose logical value. Whether to show the iteration history.
#'
#'@details
#'Please check the vignettes via \code{vignette("supc", package = "supc")} for details.
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
#'\item{iteration}{The number of iterations before convergence.}
#'\item{groups}{The partition of each iteration.}
#'\item{result}{The position of data after iterations.}
#'
#'@examples
#'\dontrun{
#'# The shape data has a structure of five clusters and a number of noise data points.
#'makecircle=function(N, seed){
#'  n=0
#'  x=matrix(NA, nrow=N, ncol=2)
#'  while (n<N){
#'    tmp=runif(2, min=0, max=1)*2-1
#'    if (sum(tmp^2)<1) {
#'       n=n+1
#'       x[n,]=tmp
#'    }
#'  }
#'  return(x)
#'}
#'
#'makedata <- function(ns, seed) {
#'  size=c(10,3,3,1,1)
#'  mu=rbind(c(-0.3, -0.3), c(-0.55, 0.8), c(0.55, 0.8), c(0.9, 0), c(0.9, -0.6))
#'  sd=rbind(c(0.7, 0.7), c(0.45, 0.2), c(0.45, 0.2), c(0.1, 0.1), c(0.1, 0.1))
#'  x=NULL
#'
#'  for (i in 1:5){
#'     tmp=makecircle(ns*size[i], seed+i)
#'     tmp[,1]=tmp[,1]*sd[i,1]+mu[i,1]
#'     tmp[,2]=tmp[,2]*sd[i,2]+mu[i,2]
#'     x=rbind(x, tmp)
#'  }
#'  
#'  tmp=runif(floor(ns/3), min=0, max=1)/5-0.1
#'  tmp=cbind(tmp, 0.8*rep(1, floor(ns/3)))
#'  x=rbind(x, tmp)
#'  x=rbind(x, matrix(1, nrow=2*ns, ncol=2)*2-1)
#'  return(x)
#'}
#'
#'shape1 <- makedata(5000, 1000)
#'dim(shape1)
#'plot(shape1)
#'
#'X.supc=supc.random(shape1, r=0.5, t="dynamic", k = 500)
#'plot(shape1, col=X.supc$cluster)
#'}
#'
#'@references
#'Shiu, Shang-Ying, and Ting-Li Chen. 2016. "On the Strengths of the Self-Updating Process Clustering Algorithm." Journal of Statistical Computation and Simulation 86 (5): 1010–1031. doi:10.1080/00949655.2015.1049605. \url{http://dx.doi.org/10.1080/00949655.2015.1049605}.
#'@export
supc.random <- function(
  x, 
  r = NULL, 
  rp = NULL, 
  t = c("static", "dynamic"), 
  k = NULL, 
  groups = NULL, 
  tolerance = 1e-4, 
  cluster.tolerance = 10 * tolerance, 
  drop = TRUE, 
  implementation = c("cpp", "R"), 
  sort = TRUE,
  verbose = (nrow(x) > 10000)
  ) {
  parameters <- .get.parameters(x, r, rp, t)
  if (is.null(groups)) parameters$groups <- rep(list(NULL), length(parameters$tau)) else {
    stopifnot(is.list(groups))
    if (!is.list(groups[[1]])) {
      parameters$groups <- rep(list(groups), length(parameters$tau))
    } else parameters$groups <- groups
  }
  if (length(k) == 1) parameters$k <- rep(k, length(parameters$tau)) else {
    stopifnot(length(k) == length(parameters$tau))
    parameters$k <- k
  }
  cl.raw <- switch(
    implementation[1], 
    "R" = .supc.random.R(x = x, parameters = parameters, tolerance = tolerance, verbose = verbose),
    "cpp" = .supc.random.cpp(x = x, parameters = parameters, tolerance = tolerance, verbose = verbose)
    )
  retval <- lapply(
    seq_along(cl.raw),
    function(.i) {
      .raw <- cl.raw[[.i]]
      cl <- .clusterize(.raw, cluster.tolerance)
      cl.size <- table(cl)
      if (sort) {
        .rank <- rank(-cl.size, ties.method = "first")
        cl <- .rank[cl]
        names(cl) <- NULL
        cl.size <- table(cl) 
      }
      cl.group <- split(seq_len(nrow(.raw)), cl)
      cl.center0 <- lapply(cl.group, function(i) {
        apply(.raw[i,,drop = FALSE], 2, mean)
      })
      cl.center <- do.call(rbind, cl.center0)
      retval <- list(
        x = x, 
        d0 = parameters$d0, 
        r = as.vector(parameters$tau[.i]), 
        t = parameters$t[[.i]], 
        cluster = cl, 
        centers = cl.center, 
        size = cl.size,
        result = structure(as.vector(.raw), .Dim = dim(.raw)),
        iteration = attr(.raw, "iteration"),
        groups = attr(.raw, "groups")
      )
      class(retval) <- "supc"
      attr(retval, "iteration") <- attr(.raw, "iteration")
      retval
    })
  if (drop && length(retval) == 1) retval[[1]] else {
    class(retval) <- "supclist"
    retval
  }
}

.supc.random.cpp <- function(x, parameters, tolerance, verbose) {
  stopifnot(length(parameters$tau) == length(parameters$t))
  lapply(seq_along(parameters$tau), function(i) {
    .current.tau <- parameters$tau[i]
    .current.t <- parameters$t[[i]]
    groups <- parameters$groups[[i]]
    if (is.null(groups)) {
      groups <- list()
    } else {
      stopifnot(is.list(groups))
    }
    k <- parameters$k[i]
    .supc.random.cpp.internal(x, .current.tau, .current.t, k, groups, tolerance, verbose)
  })
}

.supc.random.R <- function(x, parameters, tolerance, verbose) {
  lapply(seq_along(parameters$tau), function(i) {
    .current.tau <- parameters$tau[i]
    .current.t <- parameters$t[[i]]
    groups <- parameters$groups[[i]]
    k <- parameters$k[i]
    is.first <- TRUE
    t <- 0
    if (is.null(groups)) {
      groups <- list()
    } else {
      stopifnot(is.list(groups))
    }
    .group.idx <- rep(seq_len(k), ceiling(nrow(x) / k))
    repeat{
      if (is.first) {
        if (is.null(parameters$d0)) {
          parameters$d0 <- .dist(x)
        }
      }
      .T <- .current.t(t)
      t <- t + 1
      if (t > length(groups)) {
        groups[[t]] <- .group.idx[sample(nrow(x))]
      }
      group <- groups[[t]]
      .x <- x
      for(.g in seq_len(k)) {
        .idx <- group == .g
        d <- .dist(x[.idx,])
        f <- exp(-d / .T)
        f[d > .current.tau] <- 0
        f <- as.matrix(f)
        diag(f) <- exp(-0 / .T)
        f <- f / colSums(f)
        .x[.idx,] <- f %*% x[.idx,]
      }
      .difference <- max(abs(.x - x))
      if (verbose) cat(sprintf("difference: %0.8f\n", .difference))
      if (.difference < tolerance) {
        attr(x, "iteration") <- t
        attr(x, "groups") <- groups
        break
      }
      x <- .x
      is.first <- FALSE
    }
    x
  })
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
  if (is.null(x$d0)) x$d0 <- .dist(x$x)
  .hist <- freq.poly(x$d0, ...)
  lines(list(x = rep(x$r, 2), y = range(.hist$counts)), col = 2, lty = 2)
  title(sub = sprintf("r = %f", x$r))
}

#'@export
freq.poly.supclist <- function(x, ...) {
  if (is.null(x[[1]]$d0)) x[[1]]$d0 <- .dist(x[[1]]$x)
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

  argv <- list(..., x = seq_len(nrow(x$x)), y = seq_len(ncol(x$x)), z = x$x[order(x$cluster),], yaxt = yaxt, xlab = xlab, ylab = ylab, mgp = mgp)
  if (is.null(argv$main)) argv$main <- sprintf("r=%s", format(x$r, digits = title.digits))
  do.call(graphics::image, argv)
  x.at.tail <- cumsum(x$size)
  x.at.head <- c(0, utils::head(x.at.tail, -1))
  graphics::abline(v = x.at.tail[x$size > major.size] + 0.5, lty = 2)
  graphics::axis(side=2, at=seq_len(ncol(x$x)), labels=dimnames(x$x)[[2]], tick=FALSE, mgp=c(1.5,0,0))
  major.at <- apply(cbind(x.at.head[x$size > major.size], x.at.tail[x$size > major.size]), 1, mean) + 0.5
  major.label <- x$size[x$size > major.size]
  # graphics::mtext("Cluster Size", side = 3, line=1, padj = -0.5)
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
