#' Maximum Likelihood Estimation for Normal Distribution Parameters
#' Generates MLE estimates for the mean and standard deviation from a given sample vector.
#'
#' @param x a sample vector of data from a normal distribution
#' @param mu a vector of possible mean values
#' @param sig a vector of possible standard deviation values
#'
#' @return Returns Maximum Likelihood Estimates for the mean and standard deviation, and plots the MLE onto a countour graph.
#' @importFrom stats dnorm
#' @importFrom graphics contour
#' @export
#'
#' @examples
#' mymlnorm(x=c(10, 12, 13, 15, 12, 11, 10),mu=seq(5,20,length=1000),sig=seq(0.1,4,length=1000))
#'
mymlnorm=function(x,mu,sig){  #x sample vector
  nmu=length(mu) # number of values in mu
  nsig=length(sig)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log lik for normal
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of mu,
    # col2 each x with 2nd value of m
    # all with sig=sig[j]
    y=apply(z,2,sum)
    # y is a vector filled with log lik values,
    # each with a difft mu and all with the same sig[j]
    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")))
  mlx=round(mean(x),2)  # theoretical
  mly=round(sqrt((n-1)/n)*sd(x),2)
  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  invisible(list(x=x,coord=coord,maxl=maxl))
}
