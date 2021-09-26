library(supc)

# checking with reference object

## initializing datasets
X <- structure(
  c(-0.125290762148466, 2.03672866484442, 0.832874277517991, 
    6.31905616042756, 8.06590155436307, 6.8359063231764,
    3.0974858104857,  5.14766494102584, 4.1151562703307,
    0.164244239019618, 2.1187802642435,  1.18379547432164,
    6.15642726014621, 8.01491299667304, 6.60212966082732, 
    3.12396514957894, 4.9887742520942, 3.96884089865893,
    -0.0788579907420699,  1.98813732065776, 1.22000507439678,
    6.15263514969151, 7.96709528074928,  6.9493276639727,
    3.13939267508095, 5.11133263973473, 3.8622488610901, 
    -0.0610776774312712, 0.30235623369017, 1.07796864728229,
    -0.124248116108361,  -0.4429399774355, 1.22498618362862,
    2.99101327819695, 2.99676194738021,  4.18876724213706, 
    -0.294150476779855, -0.0956300110217241, 1.08358831203994,
    0.271735910305809, -0.0205575454685991, 1.07753432231187,
    2.98923899188342,  2.72458808863428, 3.91700108734006, 
    -0.141499031392424, 0.0729163924273661,  1.15370658490308, 
    -0.0224692424300456, 0.176221545290843, 1.07962117607341,
    2.87759472134985, 3.06822393828489, 3.77412738078384
  ),
  .Dim = c(27L,  2L)
)
## construct the reference answer
X.supc.ref <- structure(
  list(
    cluster = c(
      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
      8L, 9L),
    centers = structure(
      c(-0.0180254152322607, 2.046727139439, 
        1.08716939783766, 6.20911417627993, 8.0147761104807, 6.79821207689788, 
        3.1200608974441, 5.08556992438004, 3.97988236811187, -0.162390279741713, 
        0.0922984238508252, 1.10583358958467, 0.0381059301321746, -0.0860889759850034, 
        1.12872658834518, 2.95353852156743, 2.93593224005029, 3.95578772830951
      ),
      .Dim = c(9L, 2L), 
      .Dimnames = list(
        c("1", "2", "3", "4", "5", 
          "6", "7", "8", "9"),
        NULL)
      ),
    size = structure(
      c(3L, 3L, 3L, 3L, 
        3L, 3L, 3L, 3L, 3L),
      .Dim = 9L,
      .Dimnames = structure(
        list(
          cl = c("1", 
                 "2", "3", "4", "5", "6", "7", "8", "9")
          ),
        .Names = "cl"
      ),
      class = "table"
    ), 
    result = structure(
      c(-0.0180261933327141, 2.04672690185634, 1.08716459103973, 
        6.20911479359631, 8.01478639248956, 6.79821233636245, 3.120060897386, 
        5.08557036591521, 3.97988628297925, -0.0180240066479849, 2.04672737537831, 
        1.0871717225937, 6.20911329944317, 8.01477212538717, 6.79821139564722, 
        3.12006089738845, 5.08556899922362, 3.97988108345371, -0.0180260457145569, 
        2.0467271410823, 1.08717187986439, 6.2091144357991, 8.01476981396315, 
        6.79821249868371, 3.12006089755786, 5.08557040799994, 3.97987973791727, 
        -0.16238970131008, 0.0922997350318823, 1.10583302184102, 0.0381040643950639, 
        -0.0861576055475903, 1.12872663987661, 2.95353852178939, 2.9359332333143, 
        3.95579429846029, -0.162391325928923, 0.0922972350517618, 1.10583386393582, 
        0.0381086887425844, -0.0860622340754393, 1.12872647385683, 2.95353852178015, 
        2.93593015772972, 3.95578556171455, -0.162389811987271, 0.0922983014691015, 
        1.10583388297537, 0.0381050372627042, -0.0860470909879731, 1.12872665130206, 
        2.95353852113276, 2.9359333291038, 3.95578332477825), 
      .Dim = c(27L, 2L)
    ),
    iteration = 4L
  ), 
  .Names = c("cluster", "centers", "size", "result", "iteration")
)
ref.check.name <- c("cluster", "centers", "size", "result", "iteration")
class.attr.checker <- function(supc.obj) {
  # check consistency
  stopifnot(class(supc.obj) == "supc")
  attr.ref <- structure(list(names = c("x", "d0", "r", "t", "cluster", "centers", 
"size", "result", "iteration"), class = "supc"), .Names = c("names", 
"class"))
  stopifnot(isTRUE(all.equal(attributes(supc.obj), attr.ref)))
  # check with reference object
}
value.checker <- function(supc.obj) {
  . <- all.equal(supc.obj[ref.check.name], X.supc.ref)
  if (!isTRUE(.)) {
    print(.)
    stop("")
  }
}

## fitting original supc

dist.mode("stats")
obj.R <- supc1(X, r = .9, t = .75, implementation = "R")
class.attr.checker(obj.R)
value.checker(obj.R)
obj.cpp <- supc1(X, r = .9, t = .75, implementation = "cpp", verbose = TRUE)
class.attr.checker(obj.cpp)
value.checker(obj.cpp)
obj.cpp2 <- supc1(X, r = .9, t = .75, implementation = "cpp2", verbose = TRUE)
class.attr.checker(obj.cpp2)
value.checker(obj.cpp2)
# Checking with implementation of R

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
  list.check.names <- c("x", "d0", "r", "cluster", "centers", "size", "result", "iteration")
  for(supc.obj in supc.objs) {
    stopifnot(length(supc.obj) == length(ref.obj))
    for(.i in seq_along(supc.obj)) {
      . <- all.equal(
        supc.obj[[.i]][list.check.names],
        ref.obj[[.i]][list.check.names]
      )
      . <- isTRUE(.)
      if (interactive()) if (!.) browser() else stopifnot(.)
    }
    if (interactive()) {
      freq.poly(supc.obj)
      for(obj in supc.obj) {
        plot(obj)
      }
    }
  }
  NULL
}
## checking
.mode.list <- c("stats", "amap")
for(.mode in .mode.list) {
  dist.mode(.mode)
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = quantile(dist(X), seq(.1, .5, by = .1)) / 5
    ))
    objs <- list()
    for(f in .) {
      objs[[length(objs) + 1]] <- f(X)
    }
    checkers(objs)
    NULL
  })
  
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = "static"
    ))
    objs <- list()
    for(f in .) {
      objs[[length(objs) + 1]] <- f(X)
    }
    checkers(objs)
    NULL
  })
  
  local({
    . <- get.implementations(list(
      r = quantile(dist(X), seq(.1, .5, by = .1)),
      t = "dynamic"
    ))
    objs <- list()
    for(f in .) {
      objs[[length(objs) + 1]] <- f(X)
    }
    checkers(objs)
    NULL
  })
}

