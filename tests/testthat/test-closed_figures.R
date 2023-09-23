test_that("circle", {
  expect_equal(unlist(circle()$nv), 100)
  expect_equal(unlist(circle()$size.x), unlist(circle()$size.y))
})
test_that("square", {
  expect_equal(unlist(square()$nv), 4)
  expect_equal(unlist(square()$size.x), unlist(square()$size.y))
})
test_that("ellipse", {
  expect_equal(unlist(ellipse()$nv), 100)
  expect_gt(unlist(ellipse()$size.x), unlist(ellipse()$size.y))
})
test_that("luck", {
  expect_equal(unlist(luck()$nv), 4)
  expect_lt(unlist(luck()$size.x), unlist(luck()$size.y))
})
test_that("triangle", {
  expect_equal(unlist(triangle()$nv), 3)
  expect_equal(unlist(triangle()$size.x), unlist(triangle()$size.y))
})
test_that("pentagon", {
  expect_equal(unlist(pentagon()$nv), 5)
  expect_equal(unlist(pentagon()$size.x), unlist(pentagon()$size.y))
})
test_that("hexagon", {
  expect_equal(unlist(hexagon()$nv), 6)
  expect_equal(unlist(hexagon()$size.x), unlist(hexagon()$size.y))
})
