library(supc)
data("golub", package = "supc")
if (Sys.getenv("TEST_GOLUB") == "TRUE") {
  system.time(golub.cpp <- supc1(golub, r = 4, t = "dynamic", implementation = "cpp", verbose = TRUE))
  system.time(golub.r <- supc1(golub, r = 4, t = "dynamic", implementation = "R", verbose = TRUE))
  stopifnot(isTRUE(all.equal(golub.cpp, golub.r)))
}
