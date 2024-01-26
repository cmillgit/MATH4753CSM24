#' A Quadratic Function
#'
#' @param x A numeric vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' myquad(x = 1:10)
myquad <- function(x) {
  x^2 - 5*x + 7
}
