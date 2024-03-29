#' Central Limit Theorem (Poisson Distribution)
#'
#' Demonstrates the properties of the Central Limit Theorem working on the Poisson Distribution. This function generates a graphical analysis of sample means from a Poisson distribution. It creates a large number of Poisson-distributed samples, calculates their means, and then constructs a histogram with a superimposed density curve and a theoretical normal distribution curve for comparison. Additionally, it generates a barplot to display the relative frequencies of the sampled data and plots the probability mass function of the Poisson distribution for a given rate parameter lambda. The visualization layout is arranged for easy comparison of the sample distribution to the theoretical Poisson distribution.
#'
#' @param n sample size
#' @param iter number of iterations over which the sampling is performed
#' @param lambda the rate parameter of the Poisson distribution (default = 10)
#' @param ...
#'
#' @return Returns a series of plots that visualize the distribution of sample means drawn from a Poisson distribution, including a histogram with a fitted density curve and theoretical normal curve, a barplot of the sampled values' relative frequencies, and a plot of the Poisson distribution's probability mass function.
#' @export
#'
#' @examples
mycltp=function(n,iter,lambda=10,...){

  ## r-random sample from the Poisson
  y=rpois(n*iter,lambda=lambda)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax

  ## Make a suitable layout for graphing
  layout(matrix(c(1,1,2,3),nr=2,nc=2, byrow=TRUE))

  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  ## add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  # Now make a new plot
  # Since y is discrete we should use a barplot
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
