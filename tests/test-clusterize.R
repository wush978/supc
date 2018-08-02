library(supc)
library(dbscan)

for(i in 1:10) {
  m <- matrix(0, 2e2, 5)
  m[] <- rnorm(length(m))
  m2 <- supc.random(m, k = 10, rp = 0.5, t = "dynamic", tolerance = tol <- 1e-4)$result
  g1 <- dbscan::dbscan(m2, eps = tol, minPts = 1)$cluster
  g2 <- supc:::.clusterize(m2, tol)
  stopifnot(g1 == g2)
}

cl0 <- supc:::.clusterize(supc:::.test.r, 1e-3)
for(i in 1:100) {
  cl <- supc:::.clusterize(supc:::.test.r, 1e-3)
  stopifnot(isTRUE(all.equal(cl, cl0)))
}
