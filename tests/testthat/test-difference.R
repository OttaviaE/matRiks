test_that("difference distractors work", {
  m1 <- mat_apply(square(), "lty")
  dist_diff <- difference(m1)
  expect_length(dist_diff$tag, length(dist_diff$shape))
  expect_equal(class(dist_diff), "figure")
  expect_equal(any(grepl("d.ext", unlist(dist_diff$tag[[1]]))), TRUE)
  expect_equal(any(grepl("d.ext", unlist(dist_diff$tag[[2]]))), FALSE)
})
test_that("difference distractors work (opposite)", {
  m1 <- mat_apply(malta(), "lty")
  dist_diff <- difference(m1)
  expect_equal(any(grepl("d.ext", unlist(dist_diff$tag[[1]]))), FALSE)
  expect_equal(any(grepl("d.ext", unlist(dist_diff$tag[[2]]))), TRUE)
})
