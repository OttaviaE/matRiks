test_that("vertical_s", {
  expect_equal(length(unlist(vertical_s()$shape)), 2)
  expect_equal(any(grep("compose2", vertical_s()$tag)), TRUE)
  expect_equal(any(grep("ver", vertical_s()$tag)), TRUE)
  expect_equal(unlist(vertical_s()$theta.1)[1], unlist(vertical_s_inv()$theta.1)[2])
  expect_equal(unlist(vertical_s()$theta.1)[2], unlist(vertical_s_inv()$theta.1)[1])
})
test_that("vertical_s_inv", {
  expect_equal(length(unlist(vertical_s_inv()$shape)), 2)
  expect_equal(any(grep("compose2", vertical_s_inv()$tag)), TRUE)
  expect_equal(any(grep("ver", vertical_s_inv()$tag)), TRUE)
  expect_equal(unlist(vertical_s_inv()$theta.1)[1], unlist(vertical_s()$theta.1)[2])
  expect_equal(unlist(vertical_s_inv()$theta.1)[2], unlist(vertical_s()$theta.1)[1])
})
test_that("s_vertical_s", {
  expect_length(length(unlist(s_vertical_s()$shape)), 1)
  expect_equal(any(grep("simple", s_vertical_s()$tag)), TRUE)
  expect_equal(any(grep("ver", s_vertical_s()$tag)), TRUE)
  expect_equal(unlist(s_vertical_s()$theta.1)[1], unlist(s_vertical_s()$theta.1)[1])
  expect_equal(unlist(s_vertical_s()$theta.1)[2], unlist(s_vertical_s()$theta.1)[2])
})
test_that("s_vertical_s_inv", {
  expect_length(length(unlist(s_vertical_s_inv()$shape)), 1)
  expect_equal(any(grep("simple", s_vertical_s_inv()$tag)), TRUE)
  expect_equal(any(grep("ver", s_vertical_s_inv()$tag)), TRUE)
  expect_equal(unlist(s_vertical_s_inv()$theta.1)[1],
               unlist(s_vertical_s_inv()$theta.1)[1])
  expect_equal(unlist(s_vertical_s_inv()$theta.1)[2],
               unlist(s_vertical_s_inv()$theta.1)[2])
})
test_that("horizontal_s", {
  expect_equal(length(unlist(horizontal_s()$shape)), 2)
  expect_equal(any(grep("compose2", horizontal_s()$tag)), TRUE)
  expect_equal(any(grep("hor", horizontal_s()$tag)), TRUE)
  expect_equal(unlist(horizontal_s()$theta.1)[1], unlist(horizontal_s_inv()$theta.1)[2])
  expect_equal(unlist(horizontal_s()$theta.1)[2], unlist(horizontal_s_inv()$theta.1)[1])
})
test_that("horizontal_s_inv", {
  expect_equal(length(unlist(horizontal_s_inv()$shape)), 2)
  expect_equal(any(grep("compose2", horizontal_s_inv()$tag)), TRUE)
  expect_equal(any(grep("hor", horizontal_s_inv()$tag)), TRUE)
  expect_equal(unlist(horizontal_s_inv()$theta.1)[1], unlist(horizontal_s()$theta.1)[2])
  expect_equal(unlist(horizontal_s_inv()$theta.1)[2], unlist(horizontal_s()$theta.1)[1])
})
test_that("s_horizontal_s", {
  expect_length(length(unlist(s_horizontal_s()$shape)), 1)
  expect_equal(any(grep("simple", s_horizontal_s()$tag)), TRUE)
  expect_equal(any(grep("hor", s_horizontal_s()$tag)), TRUE)
  expect_equal(unlist(s_horizontal_s()$theta.1)[1], unlist(s_horizontal_s()$theta.1)[1])
  expect_equal(unlist(s_horizontal_s()$theta.1)[2], unlist(s_horizontal_s()$theta.1)[2])
})
test_that("s_horizontal_s_inv", {
  expect_length(length(unlist(s_horizontal_s_inv()$shape)), 1)
  expect_equal(any(grep("simple", s_horizontal_s_inv()$tag)), TRUE)
  expect_equal(any(grep("hor", s_horizontal_s_inv()$tag)), TRUE)
  expect_equal(unlist(s_horizontal_s_inv()$theta.1)[1],
               unlist(s_horizontal_s_inv()$theta.1)[1])
  expect_equal(unlist(s_horizontal_s_inv()$theta.1)[2],
               unlist(s_horizontal_s_inv()$theta.1)[2])
})

