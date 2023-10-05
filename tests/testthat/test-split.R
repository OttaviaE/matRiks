test_that("Split mat splits the visible elements only of the correct response", {
  m1 <- mat_apply(square())
  m2 <- mat_apply(cof(dot(),
                     size(triangle()),
                     size(luck())), "shape")
  m <- com(m1, m2)
  split_m <- split_mat(m)
  expect_equal(length(split_m), sum(m$Sq9$visible))
})
test_that("correct selectc the cell of the correct response (9 cell)", {
  m9cell <- mat_apply(cof(dot(),
                      size(triangle()),
                      size(luck())), "shape")

  c9cell <- correct(m9cell)
  expect_equal(unlist(c9cell),
               unlist(m9cell$Sq9))
})
test_that("correct selectc the cell of the correct response (4 cell)", {
  m4cell <- mat_apply(cof(dot(),
                          size(triangle()),
                          size(luck())), "shape", mat.type = 4)

  c4cell <- correct(m4cell)
  expect_equal(unlist(c4cell),
               unlist(m4cell$Sq4))
})
