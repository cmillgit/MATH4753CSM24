#' Bootstrap Resampling for Statistical Estimates with Enhanced Plotting
#'
#' This function performs bootstrap resampling to estimate the distribution of a statistic
#' and its confidence intervals and provides options for visualizing the results using
#' enhanced graphical representations. The function supports various plot types including
#' histograms and density plots.
#'
#' @param iter The number of bootstrap iterations to perform, defaults to 10,000.
#' @param x A numeric vector of data points from which to bootstrap.
#' @param fun The function to apply to each bootstrap sample. Supported functions
#'        include 'mean', 'median', etc., or any user-defined function that takes
#'        a numeric vector and returns a single value. Defaults to 'mean'.
#' @param alpha The significance level used to calculate the confidence intervals,
#'        defaults to 0.05.
#' @param cx Expansion coefficient for text in plots, defaults to 1.5.
#' @param plotType Type of plot to display the bootstrap statistics. Supported types
#'        are "histogram" and "density". Defaults to "histogram".
#' @param ... Additional graphical parameters to pass to plotting functions.
#'
#' @return Invisibly returns a list containing the computed confidence intervals,
#'         the function used for computation, and the original data vector.
#' @importFrom graphics hist plot abline segments text
#' @importFrom stats quantile density
#' @export
#' @examples
#' superboot(x = fire$DAMAGE, iter = 1000, fun = mean, plotType = 'density')
#'
superboot <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, plotType = "histogram", ...) {
  requireNamespace("graphics", quietly = TRUE)

  # Perform bootstrap and calculate statistic
  xstat <- replicate(iter, fun(sample(x, replace = TRUE)))
  pte <- fun(x)

  # Define colors and style elements
  main_color <- "#2B2D42"  # Dark blue for the main elements
  ci_color <- "#EF233C"    # Red for confidence interval
  text_color <- "#D90429"  # Dark red for text
  hist_color <- "#8D99AE"  # Grey for histograms
  line_width <- 2          # Line width for vertical lines

  # Generate plots based on the specified type
  if (plotType == "histogram") {
    hist(xstat, breaks = 30, freq = FALSE, col = hist_color, border = "white", las = 1,
         main = paste("Histogram of Bootstrap Sample Statistics", "\n", "alpha=", alpha, " iter=", iter),
         xlab = "Bootstrap Estimates", ylab = "Frequency", ...)
    abline(v = pte, lwd = line_width, col = main_color)
  } else if (plotType == "density") {
    plot(density(xstat), main = "Density Plot of Bootstrap Sample Statistics",
         xlab = "Bootstrap Estimates", ylab = "Density", col = main_color, ...)
    abline(v = pte, lwd = line_width, col = main_color)
  }

  # Calculate and annotate the confidence interval
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  if (plotType %in% c("histogram", "density")) {
    segments(ci[1], 0, ci[2], 0, lwd = line_width + 1, col = ci_color)
    text(ci[1], 0, paste("(", round(ci[1], 2), ")", sep = ""), col = text_color, cex = cx, pos = 2)
    text(ci[2], 0, paste("(", round(ci[2], 2), ")", sep = ""), col = text_color, cex = cx, pos = 4)
    if (plotType == "density") {
      text(pte, max(density(xstat)$y) / 2, round(pte, 2), cex = cx, col = text_color)
    }
  }

  invisible(list(ci = ci, fun = fun, x = x))
}

