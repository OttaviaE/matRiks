test_that("decof works", {
  m1 <- mat_apply(square())
  m2 <- mat_apply(cof(dot(),
                     size(triangle()),
                     size(luck())), "shape")
  m <- com(m1, m2)
  new_obj <- decof(m$Sq1)
  expect_error(decof(m))
  expect_equal(length(new_obj), length(m$Sq1$shape))
})
