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

## Algorithm

## Usage

Here we reproduce the section 2.3 of the paper as an usage example.

### Data

We generate the sample data as follow:


```r
set.seed(1)
mu <- list(
  x = c(0, 2, 1, 6, 8, 7, 3, 5, 4),
  y = c(0, 0, 1, 0, 0, 1, 3, 3, 4)
)
X <- lapply(1:3, function(i) {
  cbind(rnorm(9, mu$x, 1/5), rnorm(9, mu$y, 1/5))
})
X <- do.call(rbind, X)
```

### Clustering

We could apply the SUP to a matrix whose rows represent instances:


```r
library(supc)
X.supc <- supc1(X, r = 0.9, t = 0.75)
str(X.supc)
```

```
## List of 3
##  $ cluster: int [1:27] 1 2 3 4 5 6 7 8 9 1 ...
##  $ centers: num [1:9, 1:2] -0.018 2.047 1.087 6.209 8.015 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:9] "1" "2" "3" "4" ...
##   .. ..$ : NULL
##  $ size   : 'table' int [1:9(1d)] 3 3 3 3 3 3 3 3 3
##   ..- attr(*, "dimnames")=List of 1
##   .. ..$ cl: chr [1:9] "1" "2" "3" "4" ...
##  - attr(*, "class")= chr "supc"
##  - attr(*, "iteration")= num 5
```

The returned object has class "supc" which contains the cluster label of each instances, the center of the clusters, and the size of the clusters.

### Heatmap

We could draw a heatmap of the centers as follow:


```r
heatmap(X.supc$centers, Rowv = NA, Colv = NA, keep.dendro = FALSE, scale = "none")
```

![](README_files/figure-html/heatmap-1.png)<!-- -->

### The frequency polygon of the pairwise distance

```r

```


