#' Normal Distribution with Shaded Lower Tail Probability
#'
#' Plots a normal distribution curve with a specified mean and standard deviation and shades/calculates the area under the curve up to a specified upper bound.
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a upper bound up to which the area under the curve will be shaded and the probability calculated
#' @param plot logical, whether to plot of the curve and shaded region (default=TRUE)
#' @return Returns a plot and the area under the normal distribution curve from the leftmost point (considering a practical range of mean ± 3*sigma) up to the upper bound 'a'.
#' @examples
#' myncurve(0, 1, 1.96, plot = TRUE)
#' @export
myncurve = function(mu, sigma, a, plot = TRUE){
  if (plot) {
    curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
    xcurve = seq(mu - 3*sigma , a, length=1000)
    ycurve = dnorm(xcurve, mean = mu, sd = sigma)

    polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col="lightgrey")
  }

  prob = pnorm(a, mean = mu, sd = sigma)
  prob = round(prob, 4)

  print(paste("Probability: ", prob))
}
