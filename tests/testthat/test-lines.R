test_that("vertical line", {
  expect_equal(unlist(vline()$nv), 2)
  expect_equal(unlist(vline()$rotation), pi + pi / 2)
})
test_that("horizontal line", {
  expect_equal(unlist(hline()$nv), 2)
  expect_equal(unlist(hline()$rotation), pi)
})
test_that("cross", {
  expect_equal(unlist(cross()$rotation), c(pi+pi/2, pi))
  expect_length(unlist(cross()$shape), 1)
})
test_that("square4", {
  expect_equal(any(grep("compose4", unlist(square4()$tag))), TRUE)
  expect_equal(unlist(square4()$nv), rep(2, rep(length(square4()$nv))))
})
test_that("diagline", {
  expect_equal(unlist(diagline()$nv), 2)
  expect_equal(unlist(diagline()$rotation), pi-pi/4)
})
test_that("diagline_inv", {
  expect_equal(unlist(diagline_inv()$nv), 2)
  expect_equal(unlist(diagline_inv()$rotation), pi + pi / 4)
})
test_that("X", {
  expect_equal(unlist(X()$rotation), c(pi-pi/4, pi+pi/4))
  expect_length(unlist(cross()$shape), 1)
})
test_that("luck4", {
  expect_equal(any(grep("compose4", unlist(luck4()$tag))), TRUE)
  expect_equal(unlist(luck4()$nv), rep(2, rep(length(luck4()$nv))))
})
