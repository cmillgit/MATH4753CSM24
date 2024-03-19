#' A Hypergeometric Simulation
#'
#' Simulates drawing samples from a finite population without replacement, following the hypergeometric distribution. Useful for demonstrating the hypergeometric distribution's properties and outcomes under different population and sample conditions.
#'
#' @param iter number of iterations to simulate. Each iteration represents a single trial of drawing 'n' elements from the total 'N' elements
#' @param N total number of elements in the population
#' @param r number of successes in N elements
#' @param n number of elements drawn in each iteration
#'
#' @return Returns a numeric vector containing the proportions of each number of successes across the iterations and displays a barplot visualizing these proportions.
#'
#' @examples
#' myhyper(iter=1000,n=19, N=20,r=12)
#'
#' @export
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Hypergeometric Simulation", xlab="Number of successes")
  succ.tab/iter
}
