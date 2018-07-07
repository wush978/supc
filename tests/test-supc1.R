library(supc)

# checking with reference object

## initializing datasets
X <- local({
  set.seed(1)
  mu <- list(
    x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
    y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
  )
  X <- lapply(1:3, function(i) {
    cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
  })
  do.call(rbind, X)
})

## fitting original supc
dist.mode("stats")
implementations <- list(
  function(x) supc1(x, r = .9, t = .75, implementation = "cpp"),
  function(x) supc1(x, r = .9, t = .75, implementation = "R"),
  function(x) supc1(x, r = .9, t = .75, implementation = "cpp2")
)
## construct the reference answer
checkers <- local({
  X.supc.ref <- structure(list(cluster = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
  1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
  8L, 9L), centers = structure(c(-0.0180254152322607, 2.046727139439, 
  1.08716939783766, 6.20911417627993, 8.0147761104807, 6.79821207689788, 
  3.1200608974441, 5.08556992438004, 3.97988236811187, -0.162390279741713, 
  0.0922984238508252, 1.10583358958467, 0.0381059301321746, -0.0860889759850034, 
  1.12872658834518, 2.95353852156743, 2.93593224005029, 3.95578772830951
  ), .Dim = c(9L, 2L), .Dimnames = list(c("1", "2", "3", "4", "5", 
  "6", "7", "8", "9"), NULL)), size = structure(c(3L, 3L, 3L, 3L, 
  3L, 3L, 3L, 3L, 3L), .Dim = 9L, .Dimnames = structure(list(cl = c("1", 
  "2", "3", "4", "5", "6", "7", "8", "9")), .Names = "cl"), class = "table")), .Names = c("cluster", 
  "centers", "size"))
  ref.check.name <- c("cluster", "centers", "size")
  # construct the checker
  class.attr.checker <- function(supc.obj) {
    # check consistency
    stopifnot(class(supc.obj) == "supc")
    attr.ref <- structure(list(names = c("x", "d0", "r", "t", "cluster", "centers", 
  "size"), class = "supc", iteration = 4L), .Names = c("names", 
  "class", "iteration"))
    stopifnot(isTRUE(all.equal(attributes(supc.obj), attr.ref)))
    # check with reference object
  }
  value.checker <- function(supc.obj) {
    stopifnot(isTRUE(all.equal(supc.obj[ref.check.name], X.supc.ref)))
  }
  list(class.attr.checker, value.checker)
})
## checking
local({
  objs <- lapply(implementations, function(f) {
    f(X)
  })
  lapply(checkers, function(checker) {
    lapply(objs, checker)
  })
  NULL
})
  
# Checking with implementation of R

## construct implementations

get.implementations <- function(argv) {
  . <- function(implementation) {
    force(implementation)
    function(x) {
      argv$x <- x
      argv$implementation <- implementation
      do.call(supc1, argv)
    }
  }
  lapply(c("R", "cpp", "cpp2"), .)
}

## construct checkers
checkers <- function(supc.objs) {
  ref.obj <- supc.objs[[1]]
  list.check.names <- c("x", "d0", "r", "cluster", "centers", "size")
  lapply(supc.objs, function(supc.obj) {
    . <- all.equal(
      lapply(supc.obj, "[", list.check.names),
      lapply(ref.obj, "[", list.check.names)
    )
    . <- isTRUE(.)
    if (interactive()) if (!.) browser() else stopifnot(.)
  })
  NULL
}
## checking
.mode.list <- c("stats", "amap")
if (require("gputools")) {
  .mode.list <- append(.mode.list, "gputools")
}
for(.mode in .mode.list) {
  dist.mode(.mode)
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = quantile(dist(X), seq(.1, .5, by = .1)) / 5
    ))
    objs <- lapply(., function(f) {
      f(X)
    })
    checkers(objs)
    NULL
  })
  
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = "static"
    ))
    objs <- lapply(., function(f) {
      f(X)
    })
    checkers(objs)
    NULL
  })
  
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = "dynamic"
    ))
    objs <- lapply(., function(f) {
      f(X)
    })
    checkers(objs)
    NULL
  })
}
