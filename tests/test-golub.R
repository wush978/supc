library(supc)
data("golub", package = "supc")
if (Sys.getenv("TEST_GOLUB") == "TRUE") {
  print(system.time(
    golub.cpp <- supc1(golub, r = 4, t = "dynamic", implementation = "cpp", verbose = TRUE)
    ))
  cat("===\n")
  print(system.time(
    golub.cpp2 <- supc1(golub, r = 4, t = "dynamic", implementation = "cpp2", verbose = TRUE)
    ))
  cat("===\n")
  print(system.time(
    golub.r <- supc1(golub, r = 4, t = "dynamic", implementation = "R", verbose = TRUE)
    ))
  
  stopifnot(isTRUE(all.equal(golub.cpp, golub.r)))
  stopifnot(isTRUE(all.equal(golub.cpp2, golub.r)))
}
