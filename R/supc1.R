
.supc1.R <- function(x, parameters, tolerance) {
  repeat{
    d <- .dist(x)
    .T <- parameters$t()
    f <- exp(-d / .T)
    f[d > parameters$tau] <- 0
    f <- as.matrix(f)
    diag(f) <- exp(-0 / .T)
    f <- f / colSums(f)
    .x <- f %*% x
    if (sum(abs(.x - x)) < tolerance) break
    x <- .x
  }
  x
}

supc1 <- function(x, parameters = list(tau = 3.5, t = function() {0.75}), implementation = c("R", "cpp"), tolerance = 1e-4) {
  switch(
    implementation[1],
    "R" = {
      .supc1.R(x, parameters, tolerance)
    },
    "cpp" = {
      stop("TODO")
    },
    stop("unknown implementation"))
}
