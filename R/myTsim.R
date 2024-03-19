#' A Student's T Simulation
#'
#' Simulates drawing samples from a normally distributed population to calculate the Student's T statistic for each sample. It then visualizes the distribution of these T statistics across multiple iterations, comparing the empirical distribution with the theoretical T distribution. Adjustable parameters allow for customization of the visualization, including the axis limits and legend positioning. Useful in demonstrating the behavior of the Student's T statistic, especially in relation to sample size, population standard deviation, and mean.
#'
#' @param n1 The size of the sample drawn from the normally distributed population in each iteration
#' @param sigma1 The standard deviation of the population from which samples are drawn
#' @param mean1 The mean of the population from which samples are drawn
#' @param iter The number of iterations for the simulation, each producing one T statistic
#' @param ymax The maximum height of the y-axis in the histogram, for visual clarity
#' @param x x coordinate of the legend
#' @param y y coordinate of the legend
#'
#' @return Returns a histogram with a density curve representing the simulated distribution of the T statistic and overlays a theoretical T distribution curve for comparison. Also provides an invisible list containing the simulated T statistics, their summary, and standard deviation.
#' @export
#'
#' @examples
#' myTsim(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,x=2,y=0.3)
myTsim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,x=2,y=0.3,...){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  sd1=apply(data1.mat,2,sd) # sd
  ybar=apply(data1.mat,2,mean)  # mean

  w=(ybar-mean1)/(sd1/sqrt(n1))      #T stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",T," iterations= ",iter)),
       xlab=expression(paste(T, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dt(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(T==frac((bar(y)-mu),s/sqrt(n1))) #mathematical annotation -see ?plotmath
  legend(x,y,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  invisible(list(w=w,summary=summary(w),sd=sd(w),fun="T")) # some output to use if needed
}
