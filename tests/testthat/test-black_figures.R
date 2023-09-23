test_that("dot is black", {
  expect_equal(unlist(dot()$shade), "black")
})
test_that("dice is black", {
  expect_equal(unlist(dice()$shade),
               rep("black", length(unlist(dice()$size.x))))
})
test_that("cross dice is black", {
  expect_equal(unlist(cross_dice()$shade),
               rep("black", length(unlist(cross_dice()$size.x))))
})
test_that("ninja is black", {
  expect_equal(unlist(ninja()$shade),
               rep("black", length(unlist(ninja()$size.x))))
})
test_that("star is black", {
  expect_equal(unlist(star()$shade),
               rep("black", length(unlist(star()$size.x))))
})
test_that("s_biscuit is single", {
  expect_length(length(unlist(s_biscuit()$shape)), 1)
})
test_that("s_ninja is single", {
  expect_length(length(unlist(s_ninja()$shape)), 1)
})
test_that("s_star is single", {
  expect_length(length(unlist(s_ninja()$shape)), 1)
})
