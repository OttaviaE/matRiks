test_that("s_pizza_4 is single", {
  expect_length(length(unlist(s_pizza_4()$shape)), 1)
})
test_that("s_pizza_2 is single", {
  expect_length(length(unlist(s_pizza_2()$shape)), 1)
})
test_that("s_pizza_2_inv is single", {
  expect_length(length(unlist(s_pizza_2_inv()$shape)), 1)
})

