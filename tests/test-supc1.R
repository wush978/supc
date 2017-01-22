library(supc)
set.seed(1)
mu <- list(
  x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
  y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
)
X <- lapply(1:3, function(i) {
  cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
})
X <- do.call(rbind, X)
# parameters <- list(tau = 0.9, t = function() {0.75})
dist.mode("stats")
X.supc <- supc1(X, r = 0.9, t = 0.75)

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
stopifnot(isTRUE(all.equal(X.supc[ref.check.name], X.supc.ref)))
X.supc <- supc1(X, r = 0.9, t = 0.75, implementation = "cpp")
print(X.supc)
stopifnot(isTRUE(all.equal(X.supc[ref.check.name], X.supc.ref)))

X.supc <- supc1(X, r = c(0.9, 0.9), t = c(0.75, 0.75))
stopifnot(isTRUE(all.equal(lapply(X.supc, function(.) .[ref.check.name]), rep(list(X.supc.ref), 2))))
stopifnot(isTRUE(all.equal(
  supc1(X, r = quantile(dist(X), 0.85), t = 0.75),
  supc1(X, rp = 0.85, t = 0.75)
)))

list.check.names <- c("x", "d0", "r", "cluster", "centers", "size")
stopifnot(isTRUE(all.equal(
  lapply(supc1(X, r =quantile(dist(X), seq(0.1, 0.5, by = 0.1)), t = quantile(dist(X), seq(0.1, 0.5, by = 0.1)) / 5), function(obj) {
    obj[list.check.names]
  }),
  lapply(supc1(X, rp = seq(0.1, 0.5, by = 0.1), t = "static"), function(obj) {
    obj[list.check.names]
  })
)))

stopifnot(isTRUE(all.equal(
  lapply(supc1(X, r =quantile(dist(X), seq(0.1, 0.5, by = 0.1)), t = lapply(quantile(dist(X), seq(0.1, 0.5, by = 0.1)), function(.r) {
    force(.r)
    function(t) .r / 20 + (.r / 50) * t
  })), function(obj) obj[list.check.names]),
  lapply(supc1(X, rp = seq(0.1, 0.5, by = 0.1), t = "dynamic"), "[", list.check.names)
)))

stopifnot(!isTRUE(all.equal(
  supc1(X, r = 0.9, t = "dynamic"),
  supc1(X, r = 0.9, t = "static")
)))

dist.mode("amap")
X.supc <- supc1(X, r = 0.9, t = 0.75)
stopifnot(isTRUE(all.equal(X.supc[ref.check.name], X.supc.ref)))

if (require(gputools)) {
  dist.mode("gputools")
  X.supc <- supc1(X, r = 0.9, t = 0.75)
  stopifnot(isTRUE(all.equal(X.supc[ref.check.name], X.supc.ref)))
}
