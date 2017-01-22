
[![Travis-ci Status](https://travis-ci.org/wush978/supc.svg?branch=master)](https://travis-ci.org/wush978/supc)
[![Appveyor status](https://ci.appveyor.com/api/projects/status/ov2xlvx7edswtki7/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/supc)

# Self-Updating Process Clustering
Wush Wu  



This package implements the self-updating process clustering algorithms proposed in (Shiu and Chen 2016) and shows how to reproduce the examples and figures in the paper.

According to the paper, The Self-Updating Process (SUP) is a clustering algorithm that stands from the viewpoint of data
points and simulates the process how data points move and perform self-clustering. It is an iterative
process on the sample space and allows for both time-varying and time-invariant operators.

The paper shows that SUP is particularly competitive for:

- Data with noise
- Data with a large number of clusters
- Unbalanced data

This package let the user to choose the implementation of computing distance. Currently, `stats::dist`, `amap::Dist` and `gputools::gpuDist` are included. The user could choose the best fitted one according to the data. There is a c++ implementation of the main algorithm which costs about 50% times compared to the implementation in R. The vignettes also show how to plot figures for analysis purpose.

# Installation

To build the package from source, the Windows user requires [Rtools](http://cran.csie.ntu.edu.tw/bin/windows/Rtools/) and the Mac OS X user requires [gfortran](http://cran.csie.ntu.edu.tw/bin/macosx/tools/).

To get the current development version from github:

```r
# install.packages('devtools')
devtools::install_github("wush978/supc")
```

# Algorithm

## Notation

- $x_1^{(0)}, ..., x_N^{(0)} \in \mathbb{R}^{p}$ are the data to be clustered.
- $f_t$ is the truncated exponential decay function which measures the influence between two data point at $t$-th iteration.
- $\left\lVert \cdot \right\lVert_p$ is the $L_p$ norm.

## Truncated exponential decay function

At $t$-th iteration, we define the $f_t$ as:

$$\begin{equation} f_t(x_i^{(t)}, x_j^{(t)}) = \left\{ \begin{array}{lc}
exp\left(\cfrac{-\left\lVert x_i^{(t)} - x_j^{(t)} \right\lVert_2}{T(t)}\right) & \text{if } \left\lVert x_i^{(t)} - x_j^{(t)} \right\lVert_2 \leq r \\ 
0 & \text{if } \left\lVert x_i^{(t)} - x_j^{(t)} \right\lVert_2 > r
\end{array} \right. \label{eq:influence} \end{equation}$$

## Self-Updating Process

The SUP updates the data points via:

$$\begin{equation} x_i^{(t+1)} = \sum_{j=1}^N{\frac{f_t(x_i^{(t)}, x_j^{(t)})}{\sum_{k=1}^N{f_t(x_i^{(t)}, x_k^{(t)})}} x_j^{(t)}} \label{eq:sup} \end{equation}$$

The iteration stops after convergence. We compare the $\sum_{i=1}^N {\left\lVert x_i^{(t)} - x_i^{(t + 1)} \right\rVert_1}$ with `tolerance`. If the sum of the $L_1$ distance is lower than `tolerance`, then the computation stops and returns $x_1^{(t)}, ..., x_N^{(t)}$.

## Clustering

After retrieving $x_1^{(t)}, ..., x_N^{(t)}$ from SUP, we compute the connected components of the graph:

$$(V, E) = \left(\{1, 2, ..., N\}, \left\{ (i,j) | \left\lVert x_i^{(t)} - x_j^{(t)} \right\rVert_2 < tolerance \right\}\right).$$

Each connected component is a cluster.

# Usage

The function `supc1` in this package implements the SUP clustering. 

## Data

The argument `x` should be a matrix whose rows represent the instances.

## Parameters

As shown in $\eqref{eq:influence}$, there are two important parameters in SUP:

- $r$ which determines the truncation of the $f_t$.
- $T(t)$ which controls the speed of convergence.

### Control the parameter $r$

`supc1` has two parameters `r` and `rp` to control $r$. The user should provide either `r` or `rp` to `supc1`.

The value of `r` will be directly used as $r$ in $\eqref{eq:influence}$.

If `r` is a vector, then `supc1` will compute all results corresponding to different value $r$. In such case, `supc1` will return a `supclist` which contain many `supc` object corresponding to the value in the numeric vector in `r`.

The value of `rp` should between 0 and 1. The `supc1` will compute the quantile of the pairwise distance of the data as $r$ in $\eqref{eq:influence}$. If `rp` is a vector, `supc1` will compute as many objects as the number of value in `rp`.

If neither `r` nor `rp` is provided, then `supc1` will use `rp = c(0.005, 0.01, 0.03, 0.1)` as the default.

### Control the parameter $T(t)$

`supc1` has parameter `t` to control $T(t)$. 

If `t` is a function, then `supc1` will evaluate the value `t(i)` as $T(i)$ where `i` is an integer. For example, the value of `t(0)` will be $T(0)$ in $\eqref{eq:influence}$.

If `t` is a numeric value, then `supc1` set $T(t)$ as a constant whose value is `t` during the iteration.

If `t` is a character value, only the first element will be used. This element should be either `"static"` or `"dynamic"`. If `t = "static"`, `supc1` will set `t` as `function (r) { function(t) { r/5 } }` where `r` is the corresponding value of $r$ in $\eqref{eq:influence}$. This is a constant during iteration. Similarly, if `t = "dynamic"`, `supc1` will set `t` as `function (r) { function(t) { r/20 + t * (r/50) } }`. Mathematically, it is $T(t) = \frac{r}{20} + t \times \frac{r}{50}$. It will increase during the iteration.

If the user want to set different $T(t)$ with different $r$, please set `t` as a list of functions and `supc1` will calculate the result of each pair of $T(t)$ and $r$.

## Utility

### Computation of the distance

Please use `dist.mode` to select the package which will be used to compute the distance. `dist.mode("stats")`, `dist.mode("amap")`, and `dist.mode("gputools")` will pick `stats::dist`, `amap::Dist`, and `gputools::gpuDist` to compute the pairwise distance matrix respectively. It is recommended to compare the computation time of `stats::dist(x)`, `amap::Dist(x)` and `gputools::gpuDist(x)` before selecting the function.

For example, on my machine whose CPU is `Intel(R) Core(TM) i7-4820K CPU @ 3.70GHz` and GPU is `NVIDIA Corporation GK107GL [Quadro K2000]`, the computation time of different function is:

![](http://wush978.github.io/supc/dist.gif)

The results show that `stats::dist` is the best choice if the matrix is small. `gputools::gpuDist` outperforms the others if the matrix is large. If there is no GPU, `amap::Dist` might be a better choice for large matrix.





### Using implentation in `R` or in `C++`

This package provide both implementation for better maintenance. The `C++` program runs faster when the number of instances is large. 

### Verbose

If we call `supc1` with `verbose = TRUE`, then the `supc1` will show the $L_1$ distance between each iteration to the screen in real-time. Therefore, you can monitor the iteration to check if the program is dead or not.

## Example

Here we reproduce the section 2.3 of (Shiu and Chen 2016) as an usage example.

### Data

We generate the sample data first:


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

There are 9 different mean and each mean generates 3 sample data.

### Clustering

Then we could compute the SUP via `supc::supc1` with `r = 0.9` and `t = 0.75`:


```r
library(supc)
X.supc <- supc1(X, r = 0.9, t = 0.75)
```

```r
str(X.supc)
```

```
## List of 7
##  $ x      : num [1:27, 1:2] -0.125 2.037 0.833 6.319 8.066 ...
##  $ d0     :Class 'dist'  atomic [1:351] 2.19 1.49 6.44 8.2 7.08 ...
##   .. ..- attr(*, "Size")= int 27
##   .. ..- attr(*, "Diag")= logi FALSE
##   .. ..- attr(*, "Upper")= logi FALSE
##   .. ..- attr(*, "method")= chr "euclidean"
##   .. ..- attr(*, "call")= language stats::dist(x = x, method = "euclidean", diag = FALSE, upper = FALSE,      p = 2)
##  $ r      : num 0.9
##  $ t      :function (t)  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 76 9 76 24 9 24 142 142
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilealias', 'srcfile' <environment: 0x309d930> 
##  $ cluster: int [1:27] 1 2 3 4 5 6 7 8 9 1 ...
##  $ centers: num [1:9, 1:2] -0.018 2.047 1.087 6.209 8.015 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:9] "1" "2" "3" "4" ...
##   .. ..$ : NULL
##  $ size   : 'table' int [1:9(1d)] 3 3 3 3 3 3 3 3 3
##   ..- attr(*, "dimnames")=List of 1
##   .. ..$ cl: chr [1:9] "1" "2" "3" "4" ...
##  - attr(*, "class")= chr "supc"
##  - attr(*, "iteration")= int 5
```

The returned object has class "supc" which contains:

- `x`, the input data.
- `d0`, the distance matrix of the data.
- `r`, the value of $r$.
- `t`, the function of $T(t)$.
- `cluster`, the cluster label of each instances.
- `centers`, the center of the clusters.
- `size`, the size of the clusters.

#### Cluster

We could retrieve the cluster id of the instances via:


```r
X.supc$cluster
```

```
##  [1] 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9
```

This is the cluster id of the corresponding instances. For example, the cluster id of the first instance, i.e. `X[1,]`, is `X.supc$cluster[1]`. There are 9 clusters.

#### Centers

The centers of the SUP algorithm, which is the final data point after iterations, is stored in `X.supc$centers`:


```r
X.supc$centers
```

```
##          [,1]        [,2]
## 1 -0.01802542 -0.16239028
## 2  2.04672714  0.09229842
## 3  1.08716940  1.10583359
## 4  6.20911418  0.03810593
## 5  8.01477611 -0.08608898
## 6  6.79821208  1.12872659
## 7  3.12006090  2.95353852
## 8  5.08556992  2.93593224
## 9  3.97988237  3.95578773
```

Each row represents the center of the corresponding cluster id. For example, the center of cluster id 1 is `X.supc$centers[1,]`.

#### Size

The size of each cluster is stored in `X.supc$size`:


```r
X.supc$size
```

```
## cl
## 1 2 3 4 5 6 7 8 9 
## 3 3 3 3 3 3 3 3 3
```

Note that the id is ordered by the size.

### The frequency polygon of the pairwise distance

To find the better choice of $r$, the section 2.4.1 in (Shiu and Chen 2016) suggests to use the sharp valleys of the frequency polygun. The user could directly compute the frequency polygun via:


```r
freq.poly(X)
```

![](http://wush978.github.io/supc/unnamed-chunk-6-1.png)<!-- -->

There is a sharp valley around 3.5. Therefore, we compute the SUP clustering with following `r = 0.9`, `r = 3.5`, and `r = 8` as a comparison.

### `supclist`

To compute multiple parameters, the user can directly pass a vector to the corresponding parameters `r`, `rp`, or `t`:


```r
X.supcs <- supc1(X, r = c(0.9, 3.5, 8), t = 0.75)
```

The `supc1` will compute the results of all given `r`. Now, the `X.supcs` is a `supclist` object. We can retrieve all cluster id via:


```r
X.supcs$cluster
```

```
## $`r=0.900000`
##  [1] 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9
## 
## $`r=3.500000`
##  [1] 1 1 1 2 2 2 3 3 3 1 1 1 2 2 2 3 3 3 1 1 1 2 2 2 3 3 3
## 
## $`r=8.000000`
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

`r = 0.9` produces 9 clusters, `r = 3.5` produces 3 clusters and `r = 8` produces single clusters.

Also, the user can draw the frequency polygon and mark the given `r`:


```r
freq.poly(X.supcs)
```

![](http://wush978.github.io/supc/unnamed-chunk-9-1.png)<!-- -->

### Heatmap

We could draw a heatmap of the centers as follow:


```r
plot(X.supc, type = "heatmap")
```

![](http://wush978.github.io/supc/heatmap-1.png)<!-- -->

# Reference


<p>Shiu S and Chen T (2016).
&ldquo;On the strengths of the self-updating process clustering algorithm.&rdquo;
<em>Journal of Statistical Computation and Simulation</em>, <b>86</b>(5), pp. 1010-1031.
<a href="http://doi.org/10.1080/00949655.2015.1049605">doi: 10.1080/00949655.2015.1049605</a>, http://dx.doi.org/10.1080/00949655.2015.1049605, <a href="http://dx.doi.org/10.1080/00949655.2015.1049605">http://dx.doi.org/10.1080/00949655.2015.1049605</a>. 
</p>
