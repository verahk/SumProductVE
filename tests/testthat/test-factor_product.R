test_that("function returns correct product", {

  levels <- list(x = 0:3, y = 0:1, z = 0:4)
  k <- lengths(levels)

  u <- rgamma(prod(k), rep(1, prod(k)))
  p <- array(u/sum(u), k, levels)


  px   <- array(rowSums(p), k[1], levels[1])
  pyz.x <- sweep(p, 1, px, "/")

  expect_equal(factor_product(px, pyz.x), p)
  expect_equal(factor_product(pyz.x, px), p)
})
