#' Binomial Distribution Simulation and Visualization
#'
#' Simulates binomial trials a specified number of times and visualizes the distribution of the number of successes in those trials. Useful for understanding the variability and distribution of successes in repeated binomial experiments under the same conditions (number of trials and probability of success).
#'
#' @param iter number of iterations for the simulation
#' @param n number of trials in each iteration, i.e. the total number of independent experiments in a single set of binomial trials.
#' @param p probability of a success
#'
#' @return Returns a histogram showing the distribution of successes across all iterations and a numeric vector with the relative frequencies of each number of successes.
#' @export
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @examples
#' mybin(iter = 100, n = 10, p = 0.5)
mybin=function(iter=1000,n=10, p=0.5){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation",   xlab="Number of successes")
  succ.tab/iter
}
