test_that("The shade changes for single black figures", {
  my_figure <- change_color(dot())
  expect_equal(unlist(my_figure$shade), "white")
})

test_that("The shade changes for composed black figures", {
  my_figure <- change_color(biscuit())
  expect_equal(unlist(my_figure$shade), rep("white", length(biscuit()$shade)))
})

test_that("The shade changes for single figures", {
  my_figure <- change_color(square())
  expect_equal(unlist(my_figure$shade), "black")
})

test_that("The shade changes for composed figures", {
  my_figure <- change_color(malta())
  expect_equal(unlist(my_figure$shade), rep("black", length(malta()$shade)))
})
