#'@name golub
#'@title Gene expression dataset from Golub et al. (1999)
#'@description
#'Gene expression data (3051 genes and 38 tumor mRNA samples) from the leukemia microarray study of Golub et al. (1999). 
#'Each row (gene) is scaled to mean 0 and standard deviation 1.
#'
#'@return
#'\item{golub}{The matrix of scaled gene expression data.}
#'\item{golub.supc}{The result of \code{golub.supc <- supc1(golub, r = c(4, 4.3, 4.6, 4.7, 4.8), t = "dynamic")}}
#'
#'@aliases golub.supc
NULL
