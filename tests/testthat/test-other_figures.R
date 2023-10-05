test_that("vert_bow_tie", {
  expect_equal(any(grep("compose2", vert_bow_tie()$tag)), TRUE)
  expect_equal(any(grep("ver", vert_bow_tie()$tag)), TRUE)
})
test_that("hor_bow_tie", {
  expect_equal(any(grep("compose2", hor_bow_tie()$tag)), TRUE)
  expect_equal(any(grep("hor", hor_bow_tie()$tag)), TRUE)
})

test_that("s_vert_bow_tie", {
  expect_equal(any(grep("ver", s_vert_bow_tie()$tag)), TRUE)
  expect_length(length(unlist(s_vert_bow_tie()$shape)), 1)
})
test_that("s_hor_bow_tie", {
  expect_equal(any(grep("hor", s_hor_bow_tie()$tag)), TRUE)
  expect_length(length(unlist(s_hor_bow_tie()$shape)), 1)
})
test_that("malta", {
  expect_equal(any(grep("compose4", malta()$tag)), TRUE)
  expect_equal(unlist(malta()$nv), rep(3, length(unlist(malta()$nv))))
})

test_that("s_malta", {
  expect_equal(unlist(s_malta()$nv), rep(3, length(unlist(s_malta()$nv))))
  expect_length(length(unlist(s_malta()$shape)), 1)
})
test_that("axe", {
  expect_equal(any(grep("compose2", axe()$tag)), TRUE)
  expect_equal(unlist(axe()$nv), rep(100, length(unlist(axe()$nv))))
  expect_equal(unlist(axe()$rotation)[1], unlist(axe()$rotation)[2] + pi)
})
test_that("s_axe", {
  expect_length(length(unlist(s_malta()$shape)), 1)
  expect_equal(unlist(s_axe()$nv), rep(100, length(unlist(s_axe()$nv))))
  expect_equal(unlist(s_axe()$rotation)[1], unlist(s_axe()$rotation)[2] + pi)
})
test_that("maxi", {
  expect_equal(any(grep("compose4", maxi()$tag)), TRUE)
  expect_equal(unlist(maxi()$nv), rep(4, length(unlist(maxi()$nv))))
})
test_that("s_maxi", {
  expect_length(length(unlist(maxi()$shape)), 1)
  expect_equal(unlist(maxi()$nv), rep(4, length(unlist(maxi()$nv))))
})
test_that("phantom", {
  expect_equal(unlist(phantom()$vis), 0)
})

