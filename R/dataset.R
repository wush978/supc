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

#'@name shape
#'@title The Artificial Data of Five Different Clusters
#'@description
#'This artificial data was generated to have five clusters: one big circle, two small circles, and two ellipses.
#'It was to test if the clustering algorithm could identify and distinguish between the five different clusters or not.
#'The dataset is generated from the following script:
#'\preformatted{
#'
#'makecircle=function(N,seed){
#'  n=0
#'  x=NULL
#'  set.seed(seed)
#'  while (n<N){
#'        tmp=runif(2, min=-1, max=1)
#'        if (t(tmp)%*%tmp<1) {
#'            n=n+1
#'            x=rbind(x,tmp)
#'        }
#'   }
#'   return(x)
#'}
#'
#'makedata=function(n, seed){
#'  f=c(10,3,3,1,1)
#'  center=matrix(c(-0.3,-0.3, -0.55, 0.8, 0.55, 0.8, 0.9, 0, 0.9, -0.6), 
#'                nrow=5, ncol=2, byrow=TRUE)
#'  s=matrix(c(0.7, 0.7, 0.45, 0.2, 0.45, 0.2, 0.1, 0.1, 0.1, 0.1),
#'           nrow=5, ncol=2, byrow=TRUE)
#'  x=NULL
#'  for (i in 1:5){
#'      tmp=makecircle(n*f[i], seed+i)
#'      tmp[,1]=tmp[,1]*s[i,1]+center[i,1]
#'      tmp[,2]=tmp[,2]*s[i,2]+center[i,2]
#'      x=rbind(x, tmp)
#'  }
#'  line=cbind(runif(floor(n/3), min=-0.1, max=0.1), rep(0.8, floor(n/3)))
#'  noise=matrix(runif(8*n, min=-1, max=1), nrow=4*n, ncol=2)
#'  return(rbind(x, line, noise))
#'}
#'
#'x=makedata(50, 1000)
#'
#'}
#'@references
NULL