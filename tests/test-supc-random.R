library(supc)
X <- local({
  set.seed(1)
  mu <- list(
    x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
    y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
  )
  X <- lapply(1:3, function(i) {
    cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
  })
  X <- do.call(rbind, X)
})

# Checking with reference object


# parameters <- list(tau = 0.9, t = function() {0.75})
dist.mode("stats")
implementations <- local({
  .group <- list(c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L), c(1L, 1L, 
    2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 
    1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L), c(1L, 1L, 1L, 2L, 2L, 2L, 
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 
    1L, 1L, 1L, 2L, 1L), c(1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
    2L), c(2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 
    1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L), c(2L, 1L, 
    2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L), c(2L, 1L, 2L, 1L, 1L, 2L, 
    2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 
    2L, 1L, 1L, 2L, 1L), c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 
    2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
    1L), c(2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 
    1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L), c(2L, 1L, 
    1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 
    1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L), c(1L, 1L, 1L, 2L, 2L, 2L, 
    1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 
    2L, 1L, 2L, 1L, 2L), c(2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
    1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 
    2L))
  list(
    function(X) supc1(X, r = 0.9, t = 0.75),
    function(X) supc.random(X, r = 0.9, t = 0.75, k = 2, implementation = "R", groups = .group),
    function(X) supc.random(X, r = 0.9, t = 0.75, k = 2, implementation = "cpp", groups = .group)
  )
})

## Checking
local({
  check.names.ref <- c("x", "r", "cluster", "centers", "size")
  objs <- lapply(implementations, function(.) .(X))
  stopifnot(isTRUE(all.equal(
    objs[[1]]$cluster,
    objs[[2]]$cluster
  )))
  stopifnot(isTRUE(all.equal(
    objs[[2]][check.names.ref],
    objs[[3]][check.names.ref]
  )))
  lapply(objs, function(obj) stopifnot(is.null(obj$d0)))
  invisible(NULL)
})

