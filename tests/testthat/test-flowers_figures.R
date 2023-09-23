test_that("lily", {
  expect_equal(any(grep("compose4", lily()$tag)), TRUE)
})
test_that("s_lily", {
  expect_length(length(unlist(s_lily()$shape)), 1)
})
test_that("miley", {
  expect_equal(any(grep("compose4", miley()$tag)), TRUE)
})
test_that("s_miley", {
  expect_length(length(unlist(s_miley()$shape)), 1)
})
