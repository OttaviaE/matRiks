test_that("cof, concatenate figure coerently", {
  fig1 <- cof(  s_horizontal_s_inv(),
                s_horizontal_s())
  fig2 <- horizontal_eight()
  expect_equal(fig2$shape, fig1$shape)
})

test_that("cof, concatenate character coerently", {
  value1 <- "Coniglio"
  value2 <- "Rosa"
  value3 <- " "
  expect_equal(as.character(cof(value1,value3,value2)), c(value1,value3,value2))
})

test_that("com, concatenate matrices coerently", {
  fig1 <- cof(  s_horizontal_s_inv(),
                s_horizontal_s())
  fig2 <- vertical_eight()
  m1<-mat_apply(fig1,hrules = "margin.lwd")
  m2<-mat_apply(fig2,vrules = "margin.lwd")
  obj <- Map("cof",m1,m2)
  attr(obj, "class") <- "matriks"

  expect_equal(com(m1,m2),obj)
})


test_that("concatenation, concatenate stuff with other stuff", {
   num1 <- as.integer(c(19,21,3,11))
   num2 <- as.integer(c(9,20))
   a <- letters[num1]
   b <- LETTERS[num2]
  expect_equal(concatenation(num1,num2),c(num1,num2))
  expect_equal(concatenation(a,b),c(a,b))
})
