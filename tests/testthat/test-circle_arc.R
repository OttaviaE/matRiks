test_that("v_arc_right_up", {
  expect_equal(unlist(v_arc_right_up()$nv), 100)
  expect_equal(any(grepl("simple", unlist(v_arc_right_up()$tag))), TRUE)
  expect_equal(any(grepl("vert", unlist(v_arc_right_up()$tag))), TRUE)
})
test_that("v_arc_right_down", {
  expect_equal(unlist(v_arc_right_down()$nv), 100)
  expect_equal(any(grepl("simple", unlist(v_arc_right_down()$tag))), TRUE)
  expect_equal(any(grepl("vert", unlist(v_arc_right_down()$tag))), TRUE)
})
test_that("v_arc_left_up", {
  expect_equal(unlist(v_arc_left_up()$nv), 100)
  expect_equal(any(grepl("simple", unlist(v_arc_left_up()$tag))), TRUE)
  expect_equal(any(grepl("vert", unlist(v_arc_right_down()$tag))), TRUE)
})
test_that("v_arc_left_down", {
  expect_equal(unlist(v_arc_left_down()$nv), 100)
  expect_equal(any(grepl("simple", unlist(v_arc_left_down()$tag))), TRUE)
  expect_equal(any(grepl("vert", unlist(v_arc_right_down()$tag))), TRUE)
})


test_that("h_arc_right_up", {
  expect_equal(unlist(h_arc_right_up()$nv), 100)
  expect_equal(any(grepl("simple", unlist(h_arc_right_up()$tag))), TRUE)
  expect_equal(any(grepl("hor", unlist(h_arc_right_up()$tag))), TRUE)
})
test_that("h_arc_right_down", {
  expect_equal(unlist(h_arc_right_down()$nv), 100)
  expect_equal(any(grepl("simple", unlist(h_arc_right_down()$tag))), TRUE)
  expect_equal(any(grepl("hor", unlist(h_arc_right_down()$tag))), TRUE)
})
test_that("h_arc_left_up", {
  expect_equal(unlist(h_arc_left_up()$nv), 100)
  expect_equal(any(grepl("simple", unlist(h_arc_left_up()$tag))), TRUE)
  expect_equal(any(grepl("hor", unlist(h_arc_right_down()$tag))), TRUE)
})
test_that("h_arc_left_down", {
  expect_equal(unlist(h_arc_left_down()$nv), 100)
  expect_equal(any(grepl("simple", unlist(h_arc_left_down()$tag))), TRUE)
  expect_equal(any(grepl("hor", unlist(h_arc_right_down()$tag))), TRUE)
})
