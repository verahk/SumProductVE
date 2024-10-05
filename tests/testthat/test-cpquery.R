test_that("cpquery works with cpts", {
  new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
  cpts <- list(z = new_factor(c(.5, .5), 2, "z"),
               x = new_factor(c(0, 1),  2, "x"),
               y = new_factor(c(.1, .9,
                                .2, .8,
                                .3, .7,
                                .4, .6), c(2, 2, 2), c("y", "x", "z")))

  # marginal
  expect_equal(cpquery(cpts, "z"), cpts$z)

  # conditional
  py.x <-array(c(NA, NA, .3, .7), c(2, 2), list(y = NULL, x = NULL))
  expect_equal(cpquery(cpts, "y", "x"), py.x)

  # joint
  pyz.x <- aperm(cpts$y, c("y", "z", "x"))
  pyz.x[1:4] <- NA
  expect_equal(cpquery(cpts, c("y", "z"), "x"), pyz.x)
})

