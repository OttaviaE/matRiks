test_that("Response list creates a response list", {
  m1 <- mat_apply(cof(pacman(), luck(), square()), "shape", "shape")
  m2 <- mat_apply(dot(), "shade", "shade")
  m <- com(m1, m2)
  list_resp <- response_list(m)
  expect_length(list_resp, 11)
})
