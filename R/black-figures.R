#' Define the coordinates for drawing a dot
#'
#' @param size.x The length of the x-axis. Default is 2.
#' @param size.y The length of the x-axis. Default is 2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return The coordinates for drawing a black dot
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
    rotate = list(pi),
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
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing a dice with 4 dots
#'
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 13 (-13).
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 13 (-13).
#' @param shd The shading of the figure. Default is NA (transparent)
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return The coordinates for drawing a dice with 4 dots
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
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates for drawing a cross dice with 4 dots
#'
#' @param shd The shading of the figure. Default is NA (transparent)
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return The coordinates for drawing a for drawing a dice with 4 dots
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
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing a biscuit
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param size.y The length of the x-axis. Default is 15.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
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
              rotate(hexagon(size.x = size.x,
                               size.y = size.y,
                               lwd = lwd, lty = lty,
                               shd = shd), 3),
              single = T, name = "biscuit")
  value$tag <- list("compose2")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a biscuit (to be used in diff_shapes)
#'
#' @param pos.x
#' @param pos.y
#' @param size.x
#' @param size.y
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.biscuit <- function(pos.x = 0, pos.y = 0, size.x = 10,
                      size.y = 10, shd = "black", lty = 1, lwd = 3) {
  value <-cof(hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, rot = 3*pi/2,
                      lty = lty,
                      lwd = lwd),
              hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, lty = lty, lwd = lwd),
              name = "s.biscuit",
              single = TRUE)
  value$tag <- list("small", "compose2","fill")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing a single ninja star (to be used in shape())
#'
#' @param size.x The length of the x-axis. Default is 10.
#' @param size.y The length of the x-axis. Default is 15.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
s.ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "s.ninja",
              single = TRUE)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing a ninja star
#'
#' @param size.x The length of the x-axis. Default is 10.
#' @param size.y The length of the x-axis. Default is 15.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "ninja")
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing a star
#'
#' @param size.x The length of the x-axis. Default is 10.
#' @param size.y The length of the x-axis. Default is 15.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
star = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              name = "star")
}
#' Define the coordinates for drawing a single star (to be used in shape())
#'
#' @param size.x The length of the x-axis. Default is 10.
#' @param size.y The length of the x-axis. Default is 15.
#' @param shd The shading of the figure. Default is black
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
s.star = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              single = TRUE,
              name = "s.star")
}





