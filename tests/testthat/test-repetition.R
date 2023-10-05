test_that("Repetion works on 9 cell", {
  m1 <- mat_apply(square(), "lty")
  m2 <- mat_apply(cof(dot(),
                     size(triangle()),
                     size(luck())), "shape", "shape")
  m <- com(m1, m2)
  dist_rep <- repetition(m)
  expect_equal(unlist(dist_rep$r_left),
               unlist(m$Sq8))
  expect_equal(unlist(dist_rep$r_top),
               unlist(m$Sq6))
  expect_equal(unlist(dist_rep$r_diag),
               unlist(m$Sq5))
})
test_that("Repetition works on 4 cell", {
  m1 <- mat_apply(square(), "lty", mat.type = 4)
  m2 <- mat_apply(cof(dot(),
                      size(triangle()),
                      size(luck())), "shape", "shape", mat.type = 4)
  m <- com(m1, m2)
  dist_rep <- repetition(m)
  expect_equal(unlist(dist_rep$r_left),
               unlist(m$Sq3))
  expect_equal(unlist(dist_rep$r_top),
               unlist(m$Sq2))
  expect_equal(unlist(dist_rep$r_diag),
               unlist(m$Sq1))
})
test_that("Warning in repetition", {
  m1 <- mat_apply(square(), "lty")
  expect_warning(repetition(m1), "R-Top is equal to the correct response")
  m1 <- mat_apply(square(), vrules = "lty")
  expect_warning(repetition(m1), "R-left is equal to the correct response")
  m1 <- mat_apply(square(), vrules = "lty", hrules = "lty.inv")
  expect_warning(repetition(m1), "R-diag is equal to the correct response")
})
