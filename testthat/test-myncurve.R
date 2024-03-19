context("myncurve functionality")

#Probability Test
test_that("Probability calculation is correct", {
  mu <- 0
  sigma <- 1
  a <- 1.96

  output <- capture.output(prob <- myncurve(mu, sigma, a, plot = FALSE))

  printed_prob <- as.numeric(strsplit(output[1], " ")[[1]][2])

  expect_equal(printed_prob, 0.975, tolerance = 1e-4)
})
