test_that("function returns correct product", {

  levels <- list(x = 0:3, y = 0:3, z = 0:4)
  k <- lengths(levels)

  u <- rgamma(prod(k), rep(1, prod(k)))
  p <- array(u/sum(u), k, levels)


  px   <- array(rowSums(p), k[1], levels[1])
  pyz.x <- sweep(p, 1, px, "/")

  expect_equal(factor_product(px, pyz.x), p)
  expect_equal(factor_product(pyz.x, px), p)

  N <- 10^3
  data  <- sapply(levels, sample, size  = N, replace = TRUE)
  count <- table(as.data.frame(data))
  p     <- count/N

  px    <- array(rowSums(count)/N, k[1], levels[1])
  py.x  <- sweep(rowSums(count, dims = 2), 1, rowSums(count), "/")
  pz.xy <- sweep(count, 1:2, rowSums(count, dims = 2), "/")

  expect_equal(factor_product(px, py.x), rowSums(count, dims = 2)/N, ignore_attr = TRUE)
  expect_equal(factor_product(factor_product(px, py.x), pz.xy), count/N, ignore_attr = TRUE)
  expect_equal(Reduce(factor_product, list(px, py.x, pz.xy)), count/N, ignore_attr = TRUE)
  expect_equal(Reduce(factor_product, rev(list(px, py.x, pz.xy))), count/N, ignore_attr = TRUE)
  expect_equal(Reduce(factor_product, rev(list(py.x, px, pz.xy))), count/N, ignore_attr = TRUE)
  expect_equal(factor_product(py.x, px), py.x*rep(px, k[2]), ignore_attr = TRUE)
})

