library(supc)
set.seed(1)
(x <- matrix(rnorm(10), 5, 2))
d <- stats::dist(x)
.diag <- rnorm(1)
retval.ref <- local({
  d <- as.matrix(d)
  diag(d) <- .diag
  print(d[lower.tri(d, diag = TRUE)])
  attr(d, "dimnames") <- NULL
  as.matrix(d %*% x)
})
cat("retval.ref\n")
print(retval.ref)
retval <- matrix(0.0, nrow(x), ncol(x))
supc:::.test.dsymm(d, .diag, x, retval)
cat("retval\n")
print(retval)
cat("x\n")
print(x)
stopifnot(isTRUE(all.equal(retval.ref, retval)))

x2 <- matrix(1, 2, 5)
retval2.ref <- local({
  d <- as.matrix(d)
  diag(d) <- .diag
  attr(d, "dimnames") <- NULL
  as.matrix(x2 %*% d)
})
cat("retval2.ref\n")
print(retval2.ref)
retval2 <- matrix(0.0, nrow = 2, ncol = 5)
supc:::.test.dsymm(d, .diag, x2, retval2, FALSE)
cat("retval2\n")
print(retval2)
stopifnot(isTRUE(all.equal(retval2.ref, retval2)))
