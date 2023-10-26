#' Coordinates of a dot
#'
#' Define the coordinates for drawing a dot
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 2
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x
#' @param pos.x numeric, position on the x axis. Default is 0
#' @param pos.y numeric, position the y axis, Default is 0
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0
#' @param shd character, define the shading of the figure. Default is black
#'
#' @return Return the coordinates for drawing a dot
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a dot
#' dot()
#'
#' # change the shade of the dot
#'
#' dot(shd = "grey")
dot <- function(size.x = 2, size.y = size.x,  pos.x = 0, pos.y = 0, lwd = 3, lty = 1,  shd = "black", vis = 1) {
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
    tag = list(c('simple','fill', 'd.int'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a dice with four dots
#'
#' Define the coordinates for drawing four dots placed in the vertices of a square
#'
#' @inheritParams dot
#' @param pos.x numeric, position on the x axis. Default is 13 (-13)
#' @param pos.y numeric, position on the y axis. Default is 13 (-13)
#'
#' @return Return the coordinates for drawing a dice with 4 dots
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a dot
#' dice()
#' # change the shade of the dice
#' dice(shd = "grey")
dice <- function(pos.x = 13, pos.y = 13, shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x = pos.x, pos.y = pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = pos.y, shd = shd,  lwd = lwd, lty = lty),
              dot(pos.x = pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              single = TRUE, name = "dice")
  value$tag <- list(c("compose4", "d.ext"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn dice Coordinates of a cross dice with four dots
#'
#' Define the coordinates for drawing four dots placed in the vertices of a luck
#'
#' @inheritParams dice
#'
#' @return The coordinates for drawing a dice with 4 dots
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a dot
#' cross_dice()
#'
#' # change the shade of the cross dice
#'
#' cross_dice(shd = "grey")
cross_dice <- function(shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x=13,
                  pos.y=0,
                  shd = shd, lty = lty, lwd =lwd),
              dot(pos.x=-13, pos.y= 0, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x= 0,pos.y=-13, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x = 0,
                  pos.y =13, shd = shd, lty = lty, lwd =lwd),
              single = TRUE,name = "cross_dice")
  value$tag <- list(c("compose4", "d.ext"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a biscuit
#'
#' Define the coordinates for drawing a biscuit (composed of two hexagons)
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 10
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 0
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0
#' @param shd character, define the shading of the figure. Default is black
#'
#' @return Return the coordinates for drawing a biscuit
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a biscuit
#' biscuit()
#' # change the shade of the biscuit
#' biscuit(shd = "grey", lty = 0)
biscuit <- function(size.x = 10, size.y = size.x,
                   shd = "black", lwd = 3, lty = 0) {
  value = cof(hexagon(size.x = size.x,
                      size.y = size.y,
                      lwd = lwd, lty = lty,
                      shd = shd),
              rotate(hexagon(size.x = size.x,
                               size.y = size.y,
                               lwd = lwd, lty = lty,
                               shd = shd), 3))
  value$tag <- list(c("compose2", "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn biscuit Coordinates of a single biscuit
#'
#' Define the coordinates for drawing a single biscuit (composed of two hexagons), to be used in shape()
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 10
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x
#' @param pos.x numeric, position on the x axis. Default is 0
#' @param pos.y numeric, position the y axis, Default is 0
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 0
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0
#' @param shd character, define the shading of the figure. Default is black
#'
#' @return Return the coordinates for drawing a single biscuit
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single biscuit
#' s_biscuit()
#' # change the shade of the single biscuit
#' biscuit(shd = "grey", lty = 0)
s_biscuit <- function(pos.x = 0, pos.y = 0, size.x = 10, size.y = size.x, shd = "black", lty = 1, lwd = 3) {
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
  value$tag <- list(c("simple", "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a ninja star
#'
#' Define the coordinates for drawing a ninja star (composed of two lucks)
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 10
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 15
#' @inheritParams biscuit
#'
#' @return Return the coordinates for drawing a ninja star
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a ninja
#' ninja()
#' # change the shade of the ninja
#' ninja(shd = "grey", lty = 0)
ninja <- function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "ninja")
  value$tag = list(c("compose2", "rotate", "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn ninja Coordinates of a single ninja
#'
#' Define the coordinates for drawing a single ninja star (composed of two lucks (composed of two lucks), to be used in shape()
#' @inheritParams ninja
#'
#' @return Return the coordinates for drawing a single ninja
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single ninja
#' s_ninja()
#' # change the shade of the single ninja
#' s_ninja(shd = "grey", lty = 0)
s_ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "s_ninja",
              single = TRUE)
  value$tag = list(c("simple", "rotate", "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a star
#'
#' Define the coordinates for drawing a star (composed of 4 luck)
#'
#' @inheritParams ninja
#'
#' @return Return the coordinates for drawing star composed of four lucks
#' @export
#'
#' @examples
#' # get the coordinates of a star composed of four luck
#' star()
#'
#' # change the color of the star
#' draw(star(shd = "grey", lty = 0))
star <- function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              name = "star")
  value$tag = list(c("compose4",  "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn star Coordinates of a single star
#'
#' Define the coordinates for drawing a single star (composed of 4 luck), to be used in shape()
#'
#' @inheritParams star
#'
#' @return Return the coordinates for drawing a single star composed of four lucks
#' @export
#'
#' @examples
#' # get the coordinates of a single star composed of four luck
#' s_star()
#'
#' # change the color of the star
#' draw(s_star(shd = "grey", lty = 0))
s_star = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              single = TRUE,
              name = "s_star")
  value$tag = list(c("simple",  "d.int", "black.figure"))
  attr(value, "class") <- "figure"
  value
}





