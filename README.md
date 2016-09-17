This package implements the algorithm developed in <http://dx.doi.org/10.1080/00949655.2015.1049605>

## Installation

To get the current development version from github:

```r
# install.packages('devtools')
devtools::install_github("wush978/supc")
```

## Usage

```r
mu <- list(
  x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
  y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
)
X <- lapply(1:3, function(i) {
  cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
})
X <- do.call(rbind, X)
parameters <- list(tau = 0.9, t = function() {0.75})
library(supc)
X.supc <- supc1(X, parameters)
plot(X, col = 1:9) # fig.1(a) of the paper
plot(X.supc, col = 1:9) # fig.1(d) of the paper
```


