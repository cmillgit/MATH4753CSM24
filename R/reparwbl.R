#' Re-parameterization of the Weibull Density
#'
#' Re-parameterizes the Weibull distribution parameters for use in R, converting the given alpha (shape parameter) and a transformed beta (scale parameter) into the standard format expected by R's Weibull functions. It facilitates the use of Weibull distribution in statistical analysis by adjusting the parameterization to match R's conventions.
#'
#' @param a alpha (the shape parameter)
#' @param b beta^1/a (the scale parameter)
#'
#' @return Returns a list containing the re-parameterized shape and scale parameters (`ar` and `br`, respectively) for direct use in R's Weibull functions.
#' @export
#'
#' @examples
#' reparwbl(2,100)
reparwbl = function(a,b){ # a,b from text
  list(ar=a,br=b^(1/a)) # ar, br for R
}
