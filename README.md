[![Travis-ci Status](https://travis-ci.org/wush978/supc.svg?branch=master)](https://travis-ci.org/wush978/supc)
[![Appveyor status](https://ci.appveyor.com/api/projects/status/ov2xlvx7edswtki7/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/supc)

This package implements the self-updating process clustering algorithms proposed in [Shiu and Chen (2016)](http://dx.doi.org/10.1080/00949655.2015.1049605) and shows how to reproduce the examples and figures in the paper.

According to the paper, The Self-Updating Process (SUP) is a clustering algorithm that stands from the viewpoint of data
points and simulates the process how data points move and perform self-clustering. It is an iterative
process on the sample space and allows for both time-varying and time-invariant operators.

The paper shows that SUP is particularly competitive for:

- Data with noise
- Data with a large number of clusters
- Unbalanced data

This package let the user to choose the implementation of computing distance. Currently, `stats::dist`, `amap::Dist` and `gputools::gpuDist` are included. The user could choose the best fitted one according to the data. There is a c++ implementation of the main algorithm which costs about 50% times compared to the implementation in R. The vignettes also show how to plot figures for analysis purpose.

## Installation

To get the current development version from github:

```r
# install.packages('devtools')
devtools::install_github("wush978/supc")
```

## Usage

### Clustering

After installation, we could apply the SUP to a matrix whose rows represent instances:

```r
library(supc)
mu <- list(
  x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
  y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
)
X <- lapply(1:3, function(i) {
  cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
})
X <- do.call(rbind, X)
parameters <- list(tau = 0.9, t = function() {0.75})
X.supc <- supc1(X, parameters)
```

The returned object has class "supc".

### Heatmap

We could draw a heatmap of the centers as follow:

```r
heatmap(X.supc$centers, Rowv = NA, Colv = NA, keep.dendro = FALSE, scale = "none", col = rainbow(256))
```

### Pair-wise distance plot

```r
plot(X.supc)
```


