test_that("Ic size works (single matrix)", {
  m <- mat_apply(square(), "shade")
  dist_size <- ic_size(m)
  risp_correct <- correct(m)
  expect_lt(unlist(dist_size$size.x),
            unlist(risp_correct$size.x))
  expect_lt(unlist(dist_size$size.y),
            unlist(risp_correct$size.y))
  expect_equal(unlist(dist_size$pos.x),
               unlist(risp_correct$pos.x))
  expect_equal(unlist(dist_size$pos.y),
               unlist(risp_correct$pos.y))
})
test_that("Ic size works (single matrix), 4 cells", {
  m <- mat_apply(square(), "shade", mat.type = 4)
  dist_size <- ic_size(m)
  risp_correct <- correct(m)
  expect_lt(unlist(dist_size$size.x),
            unlist(risp_correct$size.x))
  expect_lt(unlist(dist_size$size.y),
            unlist(risp_correct$size.y))
  expect_equal(unlist(dist_size$pos.x),
               unlist(risp_correct$pos.x))
  expect_equal(unlist(dist_size$pos.y),
               unlist(risp_correct$pos.y))
})
test_that("Ic size works (multi-layer matrix)", {
  m1 <- mat_apply(square(), "identity")
  m2 <- mat_apply(malta(), "shade")
  m <- com(m1, m2)
  dist_size <- ic_size(m)
  risp_correct <- correct(m)
  expect_equal(all(unlist(dist_size$size.x)[-1] < unlist(risp_correct$size.x)[-1]),
            TRUE)
  expect_equal(all(unlist(dist_size$size.y)[-1] < unlist(risp_correct$size.y)[-1]),
               TRUE)
})
test_that("Ic size works (multi-layer matrix), 4 cells", {
  m1 <- mat_apply(square(), "identity", mat.type = 4)
  m2 <- mat_apply(malta(), "shade", mat.type = 4)
  m <- com(m1, m2)
  dist_size <- ic_size(m)
  risp_correct <- correct(m)
  expect_equal(all(unlist(dist_size$size.x)[-1] < unlist(risp_correct$size.x)[-1]),
               TRUE)
  expect_equal(all(unlist(dist_size$size.y)[-1] < unlist(risp_correct$size.y)[-1]),
               TRUE)
})
test_that("Ic size works (logic matrix)", {
  m <- mat_apply(square4(), "AND")
  dist_size <- ic_size(m)
  risp_correct <- correct(m)
  expect_equal(all(unlist(dist_size$size.x) < unlist(risp_correct$size.x)),
               TRUE)
  expect_equal(all(unlist(dist_size$size.y) < unlist(risp_correct$size.y)),
               TRUE)
  expect_equal(all(abs(unlist(dist_size$pos.x)[c(1:2)]) <
                     abs(unlist(risp_correct$pos.x)[c(1:2)])),
               TRUE)
  expect_equal(all(abs(unlist(dist_size$pos.y)[-c(1:2)]) <
                     abs(unlist(risp_correct$pos.y)[-c(1:2)])),
               TRUE)
})
