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
#'@references Golub, T. R., D. K. Slonim, P. Tamayo P., C. Huard C, M. Gaasenbeek M., J.P. J. P. Mesirov, H. H. Coller, et al. 1999. Molecular Classification of Cancer: Class Discovery and Class Prediction by Gene Expression Monitoring. Science 286 (5439): 531–37.
NULL

#'@name shape
#'@title The Artificial Data of Five Different Clusters
#'@description
#'This artificial data was generated to have five clusters: one big circle, two small circles, and two ellipses.
#'It was to test if the clustering algorithm could identify and distinguish between the five different clusters or not.
#'The dataset is generated from the following script:
#'
#'\preformatted{
#'makecircle <- function(N, seed) {
#'  n <- 0
#'  x <- NULL
#'  set.seed(seed)
#'  while(n < N) {
#'    tmp <- runif(2, min = -1, max = 1)
#'    if (t(tmp) \%*\% tmp < 1) {
#'      n <- n + 1
#'      x <- rbind(x, tmp)
#'    }
#'  }
#'  return (x)
#'}
#'
#'makedata <- function(n, seed) {
#'  f <- c(10, 3, 3, 1, 1)
#'  center <- matrix(
#'    c(-.3, -.3, -.55, .8, .55, .8, .9, 0, .9, -.6),
#'    nrow = 5, ncol = 2, byrow = TRUE
#'  )
#'  s <- matrix(
#'    c(.7, .7, .45, .2, .45, .2, .1, .1, .1, .1),
#'    nrow = 5, ncol = 2, byrow = TRUE
#'  )
#'  x <- NULL
#'  for (i in 1:5) {
#'    tmp <- makecircle(n * f[i], seed + i)
#'    tmp[,1] <- tmp[,1] * s[i,1] + center[i,1]
#'    tmp[,2] <- tmp[,2] * s[i,2] + center[i,2]
#'    x <- rbind(x, tmp)
#'  }
#'  line <- cbind(runif(floor(n / 3), min = -.1, max = .1), rep(.8, floor(n / 3)))
#'  noise <- matrix(runif(8 * n, min = -1, max = 1), nrow = 4 * n, ncol = 2)
#'  return(rbind(x, line, noise))
#'}
#'
#'shape <- makedata(50, 1000)
#'
#'}
#'@references Guha, S., R. Rastogi, and K. Shim. 2001. Cure: An Efficient Clustering Algorithm for Large Databases. Information Systems 26 (1): 35–38.
NULL

#'@name D31
#'@title The Artificial Data of Consisting of as Many as 31 Randomly Placed Gaussian Clusters
#'@description
#'This artificial data was generated to show the strength of SUPC.
#'Clustering \code{D31} dataset is difficult for the partition type of clustering algorithms that require an initial set.
#'However, SUP correctly identifies the 31 major clusters.
#'@references Veenman, C. J., M. J. T. Reinders, and E. Backer. 2002. A Maximum Variance Cluster Algorithm. IEEE Trans. Pattern Analysis and Machine Intelligence 24 (9): 1273–80.
NULL