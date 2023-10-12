test_that("matrix generator error test", {
  fig1 <- circle()
  fig2 <- cof(circle(),square(),s_lily())
  expect_warning(mat_apply(fig1,hrules=c("identity","identity")))
  # Logical rules on a 2x2 matrix
  expect_error(mat_apply(fig1,hrules=c("AND"),mat.type = 4))
  # Logical and visuo-spatial rules on the same layer
  expect_error(mat_apply(fig2,hrules=c("AND","shapes"),mat.type = 9))
})

