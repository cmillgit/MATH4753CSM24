#' 3D Visualization of Normal Distribution Log-Likelihood
#'
#' This function computes the log-likelihood of the normal distribution for given values
#' of mean (mu) and standard deviation (sigma) across a range of these parameters,
#' given a set of observations. It then visualizes these log-likelihood values in a 3D
#' perspective plot using a color gradient from blue to red to represent the magnitude
#' of likelihoods and highlights the maximum likelihood estimates.
#'
#' @param x A numeric vector of observations for which the likelihoods are computed.
#' @param mu A numeric vector of mean values to evaluate.
#' @param sig A numeric vector of standard deviation values to evaluate.
#' @param ... Additional graphical parameters that can be passed to `persp3d`.
#'
#' @return Returns a list containing:
#' \itemize{
#'   \item{x}{The original vector of observations.}
#'   \item{coord}{The coordinates of the maximum likelihood in the mu-sig grid.}
#'   \item{maxl}{The maximum likelihood value found.}
#'   \item{maxmu}{The mu value at the maximum likelihood.}
#'   \item{maxsigma}{The sigma value at the maximum likelihood.}
#' }
#'
#' @examples
#' mlenorm3D(x = c(5, 7, 7, 8, 10), mu = seq(5, 10, length = 100), sig = seq(0.1, 4, length = 100))
#'
#' @importFrom stats dnorm
#' @importFrom grDevices colorRampPalette
#' @import rgl
#' @export
mlenorm3D <- function(x, mu, sig, ...) {
  nmu <- length(mu)
  nsig <- length(sig)
  zz <- matrix(nrow = nmu, ncol = nsig)

  lfun <- function(x, m, p) log(dnorm(x, mean = m, sd = p))

  for (j in 1:nsig) {
    z <- outer(x, mu, lfun, p = sig[j])
    zz[, j] <- apply(z, 2, sum)
  }

  exp_zz <- exp(zz)
  maxl <- max(exp_zz)
  coord <- which(exp_zz == maxl, arr.ind = TRUE)

  # Create a color palette from blue to red
  colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(100)
  # Map exp_zz values to colors
  color_indices <- cut(exp_zz, breaks = 100, labels = FALSE)
  plot_colors <- colors[color_indices]

  # Plot using rgl
  open3d()
  persp3d(mu, sig, exp_zz, col = plot_colors, ...)

  muest <- mu[coord[1]]
  sigest <- sig[coord[2]]
  maxl_point <- exp_zz[coord]

  # Add max likelihood point
  spheres3d(muest, sigest, maxl_point, col = "red", radius = 0.05)

  return(list(x = x, coord = coord, maxl = maxl, maxmu = muest, maxsigma = sigest))
}
