
test_that("factor_product return the same result as outer() for factors with non-overlapping scope", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  x <- new_factor(1:4, c(2, 2), c("a", "b"))
  y <- new_factor(1:9, c(3, 3), c("c", "d"))

  expect_equal(factor_product(list(x, y)),
               outer(x, y))

})


test_that("factor_product with a scalar factor", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  x <- new_factor(1:4, c(2, 2), c("a", "b"))
  expect_equal(factor_product(list(x, 2)), x*2)
})

test_that("factor_product returns 1 if empty list", {
  expect_equal(factor_product(list()), 1)
})

test_that("factor_product of a length-one list of factors works", {
  x <- array(1:10, 10, list(x = NULL))
  expect_equal(factor_product(list(x)), x)
})

test_that("factor_product return the correct result with over-lapping factors", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  x <- new_factor(1:4, c(2, 2), c("a", "b"))
  y <- new_factor(1:4, c(2, 2), c("a", "c"))

  exp <- structure(c(1, 4, 3, 8, 3, 8, 9, 16),
                   dim = c(2L, 2L, 2L),
                   dimnames = list(a = NULL, b = NULL, c = NULL))
  expect_equal(factor_product(list(x, y)), exp)
})

test_that("factor_product returns error when dimension do not match", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  x <- new_factor(1:4, c(2, 2), c("a", "b"))
  y <- new_factor(1:3, c(3), c("a"))

  expect_error(factor_product(list(x, y)))
})


test_that("factor_product returns error when factor has no dimnames", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  x <- new_factor(1:5, 5, "a")
  y <- 1:5
  expect_error(factor_product(list(x, y)))
  expect_error(factor_product(list(y)))     # also for lists of length 1
})
