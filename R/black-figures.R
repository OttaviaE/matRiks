#' Coordinates of a dot
#'
#' Define the coordinates of the ellipse within which a dot can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is "black".
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a dot
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a dot
#' dot()
#'
#' # change the shade of the dot
#'
#' dot(shd = "grey")
#' }
dot <- function(size.x = 2,
                size.y = size.x,
                pos.x = 0, pos.y = 0, shd = "black",
                lty = 1,
                lwd = 3,
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
    tag = list('single','fill', 'd.int')
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a dice with four dots
#'
#' Define the coordinates for drawing four dots placed in the vertices of a square
#'
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Default is 13 (-13).
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Default is 13 (-13).
#' @param shd The shading of the figure. Default is black.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return The coordinates for drawing a dice with 4 dots
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a dot
#' dice()
#'
#' # change the shade of the dice
#'
#' dice(shd = "grey")
#' }
dice <- function(pos.x = 13, pos.y = 13, shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x = pos.x, pos.y = pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = pos.y, shd = shd,  lwd = lwd, lty = lty),
              dot(pos.x = pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              dot(pos.x = -pos.x, pos.y = -pos.y, shd = shd, lwd = lwd, lty = lty),
              single = TRUE, name = "dice")
  value$tag <- list("simple", "d.ext")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a cross dice with four dots
#'
#' Define the coordinates for drawing four dots placed in the vertices of a luck
#'
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Default is 13 (-13).
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Default is 13 (-13).
#' @param shd The shading of the figure. Default is black.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return The coordinates for drawing a dice with 4 dots
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a dot
#' cross.dice()
#'
#' # change the shade of the cross dice
#'
#' cross.dice(shd = "grey")
#' }
cross.dice <- function(shd = "black", lwd = 3, lty = 1) {
  value <-cof(dot(pos.x=13,
                  pos.y=0,
                  shd = shd, lty = lty, lwd =lwd),
              dot(pos.x=-13, pos.y= 0, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x= 0,pos.y=-13, shd = shd, lty = lty, lwd =lwd),
              dot(pos.x = 0,
                  pos.y =13, shd = shd, lty = lty, lwd =lwd),
              single = TRUE,name = "cross.dice")
  value$tag <- list("simple", "d.ext")
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a biscuit
#'
#' Define the coordinates of the ellipses within which a biscuit (composed of two hexagons) can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is black.
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a biscuit
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a biscuit
#' biscuit()
#'
#' # change the shade of the biscuit
#'
#' biscuit(shd = "grey", lty = 0)
#' }
biscuit = function(size.x = 15, size.y = size.x,
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
  value$tag <- list("compose2", "fill", "d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single biscuit
#'
#' Define the coordinates of the ellipses within which a biscuit (composed of two hexagons) can be inscribed, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is black.
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a single biscuit
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a single biscuit
#' s.biscuit()
#'
#' # change the shade of the single biscuit
#'
#' biscuit(shd = "grey", lty = 0)
#' }
s.biscuit <- function(pos.x = 0, pos.y = 0, size.x = 10,
                      size.y = size.x, shd = "black", lty = 1, lwd = 3) {
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
  value$tag <- list("simple","fill", "d.int")
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a ninja star
#'
#' Define the coordinates of the ellipses within which a ninja (composed of two lucks) can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is black.
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a ninja
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a ninja
#' ninja()
#'
#' # change the shade of the ninja
#'
#' ninja(shd = "grey", lty = 0)
#' }
ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "ninja")
  value$tag = list("compose2", "fill", "d.int")
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a single ninja
#'
#' Define the coordinates of the ellipses within which a ninja (composed of two lucks) can be inscribed, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is black.
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a single ninja
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a single ninja
#' s.ninja()
#'
#' # change the shade of the single ninja
#'
#' ninja(shd = "grey", lty = 0)
#' }
s.ninja = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              name = "s.ninja",
              single = TRUE)
  value$tag = list("simple", "fill", "d.int")
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
#' @return The coordinates of a star composed of four lucks
#' @export
#'
#' @examples
#' \dontrun{
#' # get the coordinates of a star composed of four luck
#' star()
#'
#' # change the color of the star
#' draw(star(shd = "grey", lty = 0))
#'
#' }
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
#' @return The coordinates of a single star composed of four lucks
#' @export
#'
#' @examples
#' \dontrun{
#' # get the coordinates of a single star composed of four luck
#' s.star()
#'
#' # change the color of the star
#' draw(s.star(shd = "grey", lty = 0))
#'
#' }
s.star = function(size.x = 10, size.y = 15, shd = "black", lwd = 3, lty = 0) {
  value = cof(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 3),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 4),
              rotate(luck(size.x = size.x, size.y = size.y, shd = shd, lwd = lwd, lty = lty), 6),
              single = TRUE,
              name = "s.star")
}





