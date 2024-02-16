test_that("Rotation works (theta.1 and theta.2)", {
  for(n in 1:9)
  {
  num <- 4
  fig1 <- pacman()
  fig2 <- rotate(pacman(), n = n)
  expect_equal(fig2$rotation[[1]], fig1$rotation[[1]] + (n-1)*pi/num)
  expect_equal(fig2$theta.1[[1]], fig1$theta.1[[1]] + (n-1)*pi/num)
  expect_equal(fig2$theta.2[[1]], fig1$theta.2[[1]] + (n-1)*pi/num)
  }
})


test_that("Is getting bigger? Increasing size", {
  for(n in 1:9)
  {
    fig1 <- square()
    fig2 <- size(fig1, n = n ,rule = "size.inv")
    expect_equal(fig2$size.x[[1]], fig1$size.x[[1]]*(n*.6))
    expect_equal(fig2$size.y[[1]], fig1$size.y[[1]]*(n*.6))
    expect_equal(fig2$pos.x[[1]], fig1$pos.x[[1]]/(n*.6))
    expect_equal(fig2$pos.y[[1]], fig1$pos.y[[1]]/(n*.6))
  }
})

test_that("Size doesn't matter! Decreasing size", {
  for(n in 1:9)
  {
    fig1 <- square()
    fig2 <- size(fig1, n = n ,rule = "size")
    expect_equal(fig2$size.x[[1]], fig1$size.x[[1]]/(n*.9))
    expect_equal(fig2$size.y[[1]], fig1$size.y[[1]]/(n*.9))
    expect_equal(fig2$pos.x[[1]], fig1$pos.x[[1]]/(n*.9))
    expect_equal(fig2$pos.y[[1]], fig1$pos.y[[1]]/(n*.9))
  }
})

test_that("Change shape straight", {
  for(n in 1:9)
  {
    fig1 <- cof(square(), square(),s_lily())
    fig2 <- shape(fig1, n = n ,rule = "shape")
    index<-c(1:3,1:3,1:3) #TR-LL
    expect_equal(which(fig2$visible==1), index[n])
  }
})

test_that("Change shape reverse", {
  for(n in 1:9)
  {
    fig1 <- cof(square(), square(),s_lily())
    fig2 <- shape(fig1, n = n ,rule = "shape.inv")
    index<-c(3:1,3:1,3:1) #TR-LL
    expect_equal(which(fig2$visible==1), index[n])
  }
})

test_that("Change shade straight", {
  for(n in 1:9)
  {
    fig1 <- cof(square())
    fig2 <- shade(fig1, n = n ,rule = "shade")
    index <- rep(c("white","grey","black"),3)

    position<-unlist(lapply(fig1$shade, function(x,i,n)
    {
      lx <- length(x)
      pos <- index==x[1]
      if(is.na(sum(pos)))
      {
        return(n)
      }else{
        pos <- which(pos)
        return(rep(pos[1]+n,lx))
      }
    },index,n))
    expect_equal(unlist(fig2$shade), index[position])
  }
})

test_that("Change margin width straight", {
  for(n in 1:3)
  {
    fig1 <- cof(square())
    fig2 <- margin(fig1, n = n ,rule = "margin.lwd")
    index<-c(1:3,1:3,1:3) #TR-LL
    expect_equal(fig2$lwd, lapply(fig1$lwd,function(x,i,n){i[x+n]+1},index,n))
  }
})

test_that("Change margin  width reverse", {
  for(n in 1:3)
  {
    fig1 <- cof(circle())
    fig2 <- margin(fig1, n = n ,rule = "margin.lwd.inv")
    index<-c(3:1,3:1,3:1) #TR-LL
    expect_equal(fig2$lwd, lapply(fig1$lwd,function(x,i,n){i[x+n]+1},index,n))
  }
})
