#' A Chi-square Simulation
#'
#' Simulates and visualizes the chi-square distribution of sample variances from a normally distributed population. It compares the empirical distribution of the chi-square statistic, based on sample variance, with the theoretical chi-square distribution over a specified number of iterations. Demonstrates the convergence of the empirical distribution to the theoretical distribution under the central limit theorem.
#'
#' @param n1 size of sample from a normally distributed population
#' @param sigma1 standard deviation of the population
#' @param mean1 mean of the population
#' @param iter number of sampling iterations
#' @param ymax height of the y axis
#' @param x x coordinate of the legend
#' @param y y coordinate of the legend
#'
#' @return Returns a histogram comparing the simulated and theoretical densities
#' @export
#'@importFrom stats rnorm var dchisq sd density
#'@importFrom graphics hist lines curve legend

#' @examples
#' mychisim(iter=1000, mean1= 10, sigma1 = 4, ymax=0.15, x = 20, y = 0.1)
mychisim=function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,x=20, y=0.1){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  ssq1=apply(data1.mat,2,var) # ssq1 is s squared

  w=(n1-1)*ssq1/sigma1^2      #chi-sq stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #mathematical annotation -see ?plotmath
  legend(x,y,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  invisible(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}
