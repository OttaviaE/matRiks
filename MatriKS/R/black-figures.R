#' Define the coordinates of a dot
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param lty
#' @param lwd
#' @param vis
#'
#' @return
#' @export
#'
#' @examples One day not today
dot <- function(size.x = 2,
                size.y = 2,
                pos.x = 0, pos.y = 0, shd = "black",
                lty = lty,
                lwd = lwd,
                vis = 1) {
  value <- list(
    shape = "dot",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(100),
    shade = list(shd),
    visible = vis,
    tag = list('single','fill')
  )
  attr(value, "class") <- "cell"
  value
}

#' Define the coordinates of a dice with 4 dots
#'
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
dice <- function(pos.x = 13, pos.y = 13, shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x = pos.x, pos.y = pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = pos.y, shd = shd,  lwd = lwd, lty = lty),
              dot(pos.x = pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              single = TRUE, name = "dice")
  value$tag <- list("simple")
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a cross dice with 4 dots
#'
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
cross.dice <- function(shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x=13,
                  pos.y=0,
                  shd = shd, lty = lty, lwd =lwd),
              dot(pos.x=-13, pos.y= 0, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x= 0,pos.y=-13, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x = 0,
                  pos.y =13, shd = shd, lty = lty, lwd =lwd),
              single = TRUE,name = "cross.dice")
  value$tag <- list("simple")
  attr(value, "class") <- "cell"
  value
}

#' Define the coordinates of a biscuit
#'
#' @param size.x
#' @param size.y
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
biscuit = function(size.x = 15, size.y = 15,
                   shd = "black", lwd = 3, lty = 0) {
  value = cof(hexagon(size.x = size.x,
                      size.y = size.y,
                      lwd = lwd, lty = lty,
                      shd = shd),
              rotation(hexagon(size.x = size.x,
                               size.y = size.y,
                               lwd = lwd, lty = lty,
                               shd = shd), 3),
              single = T, name = "biscuit")
  value$tag <- list("compose2")
  attr(value, "class") <- "cell"
  value
}

#' Define the coordinates of a ninja star
#'
#' @param size.x
#' @param size.y
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotation(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "ninja",
              single = TRUE)
  value$tag = list("compose2")
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a star
#'
#' @param size.x
#' @param size.y
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
star = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotation(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotation(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotation(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              single = TRUE,
              name = "star")
}




