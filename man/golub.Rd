% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{golub}
\alias{golub}
\alias{golub.supc}
\title{Gene expression dataset from Golub et al. (1999)}
\value{
\item{golub}{The matrix of scaled gene expression data.}
\item{golub.supc}{The result of \code{golub.supc <- supc1(golub, r = c(4, 4.3, 4.6, 4.7, 4.8), t = "dynamic")}}
}
\description{
Gene expression data (3051 genes and 38 tumor mRNA samples) from the leukemia microarray study of Golub et al. (1999). 
Each row (gene) is scaled to mean 0 and standard deviation 1.
}
\references{
Golub, T. R., D. K. Slonim, P. Tamayo P., C. Huard C, M. Gaasenbeek M., J.P. J. P. Mesirov, H. H. Coller, et al. 1999. Molecular Classification of Cancer: Class Discovery and Class Prediction by Gene Expression Monitoring. Science 286 (5439): 531–37.
}
