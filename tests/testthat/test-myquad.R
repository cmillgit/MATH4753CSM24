test_that("multiplication works", {
  v<- myquad(1:20)
  expect_length(object = v, n= 20)
  expect_equal(2 * 2, 4)
})
