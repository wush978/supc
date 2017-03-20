
[![Travis-ci Status](https://travis-ci.org/wush978/supc.svg?branch=master)](https://travis-ci.org/wush978/supc)
[![Appveyor status](https://ci.appveyor.com/api/projects/status/ov2xlvx7edswtki7/branch/master?svg=true)](https://ci.appveyor.com/project/wush978/supc)

This package implements the self-updating process clustering algorithms proposed by (Shiu and Chen 2016). This document shows how to reproduce the examples and figures in the paper.

According to the paper, The Self-Updating Process (SUP) is a clustering algorithm that stands from the viewpoint of data
points and simulates the process how data points move and perform self-clustering. It is an iterative
process on the sample space and allows for both time-varying and time-invariant operators.

The paper shows that SUP is particularly competitive for:

- Data with noise
- Data with a large number of clusters
- Unbalanced data

# Installation

To build the package from source, the Windows user requires [Rtools](http://cran.csie.ntu.edu.tw/bin/windows/Rtools/) and the Mac OS X user requires [gfortran](http://cran.csie.ntu.edu.tw/bin/macosx/tools/).

To install the package from CRAN:

```r
install.packages("supc")
```

To get the current development version from github:

```r
# install.packages('remotes')
remotes::install_github("wush978/supc")
```

For details, please visit <http://rpubs.com/wush978/supc>

# Reference

<p>Shiu S and Chen T (2016).
&ldquo;On the strengths of the self-updating process clustering algorithm.&rdquo;
<em>Journal of Statistical Computation and Simulation</em>, <b>86</b>(5), pp. 1010-1031.
<a href="http://doi.org/10.1080/00949655.2015.1049605">doi: 10.1080/00949655.2015.1049605</a>, http://dx.doi.org/10.1080/00949655.2015.1049605, <a href="http://dx.doi.org/10.1080/00949655.2015.1049605">http://dx.doi.org/10.1080/00949655.2015.1049605</a>. 
</p>
