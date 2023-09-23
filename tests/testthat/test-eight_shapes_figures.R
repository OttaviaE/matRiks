test_that("vertical_eight", {
  expect_equal(any(grep("compose2", vertical_eight()$tag)), TRUE)
  expect_equal(any(grep("ver", vertical_eight()$tag)), TRUE)
})
test_that("horizontal_eight", {
  expect_equal(any(grep("compose2", horizontal_eight()$tag)), TRUE)
  expect_equal(any(grep("hor", horizontal_eight()$tag)), TRUE)
})

test_that("s_vertical_eight", {
  expect_equal(any(grep("ver", s_vertical_eight()$tag)), TRUE)
  expect_length(length(unlist(s_vertical_eight()$shape)), 1)
})
test_that("s_horizontal_eight", {
  expect_equal(any(grep("hor", s_horizontal_eight()$tag)), TRUE)
  expect_length(length(unlist(s_horizontal_eight()$shape)), 1)
})
