test_that("Flip the correct response (single matrix)", {
  m1 <- mat_apply(pacman(), hrules = "rotate", vrules = "shade")
  dist_flip <- ic_flip(m1)
  expect_equal(class(dist_flip), "figure")
  expect_equal(unlist(dist_flip), unlist(rotate(correct(m1), 2)))
  m2 <- mat_apply(s_vert_bow_tie(), hrules = "lty", vrules = "shade")
  expect_equal(any(grepl("vert", unlist(correct(m2)))), TRUE)
  expect_equal(any(grepl("hor", unlist(ic_flip(m2)))), TRUE)
})
test_that("Can't flip the correct response (single matrix)", {
  m1 <- mat_apply(malta(), hrules = "size", vrules = "shade")
  expect_warning(ic_flip(m1), "Can't rotate, sorry!")
})
test_that("Flip the correct response (multi-layer matrix)", {
  m1 <- mat_apply(malta(), hrules = "size", vrules = "shade")
  m2 <- mat_apply(square())
  m <- com(m2, m1)
  dist_flip <- ic_flip(m)
  risp_correct <- correct(m)
  fig <- get(risp_correct$shape[[1]])
  new_img <- replace(correct(m),
                     1,
          rotate(fig(), 2))
  expect_equal(unlist(dist_flip), unlist(new_img))
})
