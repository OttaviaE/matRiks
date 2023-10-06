test_that("Ic neg works when image can't change color", {
  m <- mat_apply(miley(), "size", "lty")
  expect_warning(ic_neg(m), "Can't change color, sorry!")
})
test_that("Ic neg works when image can't change color, 4 cell", {
  m <- mat_apply(miley(), "size", "lty", mat.type = 4)
  expect_warning(ic_neg(m), "Can't change color, sorry!")
})
test_that("Ic neg works when image can change color (single layer)", {
  m <- mat_apply(square(), "shade", "lty")
  dist_neg <- ic_neg(m)
  expect_equal(unlist(dist_neg$shade),
               rep("white", length(unlist(dist_neg$shade))))
})
test_that("Ic neg works when image can change color (multi layer)", {
  m1 <- mat_apply(luck(), "identity")
  m2 <- mat_apply(dot(), "shade")
  m <- com(m1, m2)
  dist_neg <- ic_neg(m)
  split_correct <- correct(m)
  expect_equal(unlist(dist_neg$shade)[length(split_correct$shape)],
              "white")
})
