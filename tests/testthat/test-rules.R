test_that("Rotation works (theta.1 and theta.2)", {
  n <- 4
  num <- 4
  fig1 <- pacman()
  fig2 <- rotate(pacman(), n = n)
  expect_equal(fig2$rotation[[1]], fig1$rotation[[1]] + (n-1)*pi/num)
  expect_equal(fig2$theta.1[[1]], fig1$theta.1[[1]] + (n-1)*pi/num)
  expect_equal(fig2$theta.2[[1]], fig1$theta.2[[1]] + (n-1)*pi/num)
})
