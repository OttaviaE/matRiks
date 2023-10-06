test_that("Ic inc throws a wanring on single layer matriks", {
  m <- mat_apply(dot(), "shade", "shade")
  expect_warning(ic_inc(m), "IC-Inc cannot be obtained with a single figure")
})
test_that("Ic inc throws a wanring on single layer matriks 4 cells", {
  m <- mat_apply(dot(), "shade", "shade", mat.type = 4)
  expect_warning(ic_inc(m), "IC-Inc cannot be obtained with a single figure")
})
test_that("Ic inc removes an element from a multi-layer matrix", {
  m1 <- mat_apply(cof(pacman(), luck(), square()), "shape", "shape")
  m2 <- mat_apply(dot(), "shade", "shade")
  m <- com(m1, m2)
  dist_inc <- ic_inc(m)
  expect_lt(sum(dist_inc$visible), sum(correct(m)$visible))
})
