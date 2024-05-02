#' Plane Ticket Sales Optimization
#'
#' This function computes the optimal number of plane tickets to sell to prevent overbooking beyond a specified probability, using discrete binomial and normal distribution approximations. It generates comparative plots for each approach, pinpointing the ticket quantity where the likelihood of overbooking is just below the acceptable threshold.
#'
#' @param N the number of seats on the flight
#' @param gamma the probability of overbooking (i.e, P(s > N) where s = total number of people with tickets who show up)
#' @param p the probability that a person with a ticket shows up for the flight
#'
#' @return Returns descriptive plots and prints the optimal tickets sold (both discrete and continuous)
#' @importFrom stats pbinom pnorm optimize
#' @importFrom graphics abline
#' @export
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95){
  # Finding optimal n (discrete)
  lambda = seq(N, N*1.1, by = 1) # lambda is defined as a vector of possible values of n
  objective1 = 1 - gamma - pbinom(N, lambda, p) # the absolute value of the objective function (discrete)
  obj1.min = which.min(abs(objective1)) # the index of the smallest value in the objective function (discrete)
  # this index corresponds to the optimal value of n within lambda (discrete)
  nd = lambda[obj1.min] # applying to give us the optimal number of tickets (discrete)

  # Plotting Discrete
  plot(lambda, objective1, type = "b", pch = 19, col = "blue",
       main = bquote("Objective vs n to Find Optimal Tickets Sold (" * .(nd) *
                       ") " * gamma * "=" * .(gamma) * "," * N * "=" * .(N) *
                       " (Discrete Case)"),
       xlab = "n", ylab = "Objective", ylim = c(0, 1))
  abline(h = 0, col = "red", lwd = 1) # adds a horizontal line at 0
  abline(v = nd, col = "red", lwd = 1) # adds a vertical line at nd

  # Finding optimal n using the normal approximation to binomial
  objective2 <- function(n){ # defining the function for optimization
    mean = n * p
    sd = sqrt(n * p * (1 - p))
    abs(1 - gamma - pnorm(N + 0.5, mean, sd)) # the absolute difference
  }

  obj2.min = optimize(objective2, c(N, N * 1.1), maximum = FALSE)
  nc = obj2.min$minimum

  results <- list(
    "Optimal Number of Tickets (Discrete)" = nd,
    "Optimal Number of Tickets (Normal Approximation)" = nc,
    "Number of Seats on the Flight (N)" = N,
    "Probability of a Person Showing Up (p)" = p,
    "Probability of Overbooking (gamma)" = gamma
  )

  print(results)
  # Constructing the normal approximation plot
  nc_values = seq(N, N*1.1, length.out = 1000)
  objective2_values = sapply(nc_values, function(n) {
    mean = n * p
    sd = sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N + 0.5, mean, sd) # don't want absolute value for plotting
  })

  plot(nc_values, objective2_values, type = "l", col = "black",
       main = bquote("Objective vs n to Find Optimal Tickets Sold (" * .(nc) *
                       ") " * gamma * "=" * .(gamma) * "," * N * "=" * .(N) *
                       " (Normal Approximation)"),
       xlab = "n", ylab = "Objective", ylim = range(objective2_values))
  abline(h = 0, col = "blue", lwd = 1) # adds a horizontal line at 0
  abline(v = nc, col = "blue", lwd = 1) # adds a vertical dashed line at nc
}

