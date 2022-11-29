test_that("sum product elim works", {
  levels <- list(x = 0:3, y = 0:1, z = 0:4)
  k <- lengths(levels)

  u <- rgamma(prod(k), rep(1, prod(k)))
  p <- array(u/sum(u), k, levels)

  pyz  <- colSums(p, dims = 1)
  pz   <- array(colSums(pyz, dims = 1), k[3], levels[3])
  py.z <- sweep(pyz, 2, pz, "/")
  px.yz <- sweep(p, 2:3, pyz, "/")

  factors <- list(pz, py.z, px.yz)
  expect_equal(p, sum_product_ve(rev(factors), NULL))
  expect_equal(pyz, sum_product_ve(rev(factors), "x"))
  expect_equal(pz, sum_product_ve(rev(factors), c("x", "y")))
  expect_equal(array(1, 1, list("." = NULL)), sum_product_ve(rev(factors), c("x", "y", "z")))
})
