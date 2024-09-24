test_that("multiplication works", {

  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  z <- new_factor(c(.5, .5), 2, "z")
  x <- new_factor(c(0, 1),  2, "x")
  y <- new_factor(c(.1, .9,
                    .2, .8,
                    .3, .7,
                    .4, .6), c(2, 2, 2), c("y", "x", "z"))
  factors <- list(z, x, y)

  # check that conditional dist of y sums to 1
  expect_equal(colSums(y), rep(1, 4), ignore_attr = T)

  # eliminate "y"
  expect_equal(sum_product_elim_var(factors, "y"),
               list(z, x, new_factor(1, c(2, 2), c("x", "z"))))

  # sum product over y
  expect_equal(sum_product_ve(factors, "y"), outer(z, x))

  # sum product over all vars - return constant equal to 1
  expect_equal(sum_product_ve(factors, c("x", "y", "z")), 1)

  # sum product over all vars and a constant
  expect_equal(sum_product_ve(c(factors, 2), c("x", "y", "z")), 2)

  # sum product
  factors2  <- c(factors, list(1:2))
  factors2  <- sum_product_elim_var(factors2, "y")
  factors2  <- sum_product_elim_var(factors2, "x")
  factors2  <- sum_product_elim_var(factors2, "z")

  expect_error(sum_product_ve(c(factors, list(1:2)), c("x", "y")))
  expect_error(sum_product_ve(c(factors, list(1:2)), c("x", "y", "z")))



})
