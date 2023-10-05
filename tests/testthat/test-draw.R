test_that("draw cannot be applied to list", {
  fig1 <- list(
    shape = "Not a figure",
    size.x  = list(3),
    size.y  = list(3),
    theta.1  = list(3*pi/4),
    theta.2  = list(5*pi/4),
    rotation = list(pi),
    pos.x  = list(5),
    pos.y  = list(5),
    lty  = list(3),
    lwd  = list(3),
    num  = list(2),
    nv  = list(100),
    shade  = list(NA),
    visible = 1,
    tag=list("Not a figure")
  )
  expect_error(draw(fig1))
})

test_that("draw figure is invisible", {
  fig1<-pacman()
  expect_invisible(draw(fig1))
})

test_that("draw matriks is invisible", {
  fig1 <- cof(pacman(),square(), s_lily())
  m <- mat_apply(fig1, hrules="shape",vrules = "size")
            expect_invisible(draw(m))
})
