test_that("Wrong principle 4 cells equal rules H and V", {
  m1 <- mat_apply(square(), hrules = "shade", vrules = "shade", mat.type = 4)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)),
               sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))))
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof(size(m1$Sq3), rotate(dist_wp$wp_copy))))
})
test_that("Wrong principle 4 cells equal rules H and V (can't rotate)", {
  m1 <- mat_apply(lily(), hrules = "lty", vrules = "lty", mat.type = 4)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)) == sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))),
               FALSE)
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof((dist_wp$wp_copy), size(m1$Sq3))))
})
test_that("Wrong principle 4 cells different rules H and V", {
  m1 <- mat_apply(square(), hrules = "shade", vrules = "size", mat.type = 4)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)),
               sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))))
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof(size(m1$Sq2), rotate(dist_wp$wp_copy))))
})
test_that("Wrong principle 4 cells differen rules H and V (can't rotate)", {
  m1 <- mat_apply(lily(), hrules = "lty", vrules = "size", mat.type = 4)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)) == sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))),
               FALSE)
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof((dist_wp$wp_copy), size(m1$Sq2))))
})
test_that("Wrong principle 9 cells equal rules H and V", {
  m1 <- mat_apply(square(), hrules = "shade", vrules = "shade", mat.type = 9)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq3))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)),
               sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))))
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof(size(m1$Sq8), rotate(dist_wp$wp_copy))))
})
test_that("Wrong principle 9 cells equal rules H and V (can't rotate)", {
  m1 <- mat_apply(lily(), hrules = "lty", vrules = "lty", mat.type = 9)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq3))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)) == sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))),
               FALSE)
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof((dist_wp$wp_copy), size(m1$Sq8))))
})
test_that("Wrong principle 9 cells different rules H and V", {
  m1 <- mat_apply(square(), hrules = "shade", vrules = "size", mat.type = 9)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)),
               sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))))
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof(size(m1$Sq5), rotate(dist_wp$wp_copy))))
})
test_that("Wrong principle 9 cells different rules H and V (can't rotate)", {
  m1 <- mat_apply(lily(), hrules = "lty", vrules = "size", mat.type = 9)
  dist_wp <- wp(m1)
  expect_equal(class(dist_wp), "responses")
  expect_equal(unlist(dist_wp$wp_copy), unlist(m1$Sq1))
  expect_equal(length(unlist(dist_wp$wp_copy$nv)) == sum(grepl("rotate", unlist(dist_wp$wp_copy$tag))),
               FALSE)
  expect_equal(unlist(dist_wp$wp_matrix), unlist(cof((dist_wp$wp_copy), size(m1$Sq5))))
})
