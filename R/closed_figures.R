#' Coordinates of a circle
#'
#' Define the coordinates of the ellipse within which a circle can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a circle
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a circle
#' circle()
#'
#' # change the coordinates for drawing a smaller circle
#'
#' circle(size.x = 5)
#' }
circle <- function(size.x = 10,
                   size.y = size.x,
                   pos.x = 0, pos.y = 0, shd = NA,
                   vis = 1,
                   lty = 1,
                   lwd = 3) {
  value <- list(
    shape = "circle",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(0),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(100),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'fill', 'd.ext'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of an ellipse
#'
#' Define the coordinates an ellipse
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 7.
#' @param rot Rotation of the figure. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates fro drawing a ellipse
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing an ellipse
#' ellipse()
#'
#' # change the coordinates for drawing a smaller ellipse
#'
#' ellipse(size.x = 5, size.y = 3)
#' }
ellipse <- function(size.x=10,
                    size.y=7,
                    rot=0,
                    shd=NA, pos.x = 0, pos.y = 0,
                    vis = 1,
                    lty = 1,
                    lwd = 3) {
  value <- list(
    shape = "ellipse",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(100),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple', 'fill', 'rotate', 'd.ext'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a triangle
#'
#' Define the coordinates  of the ellipse within which a triangle can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param rot Rotation of the figure. Default is pi/2.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a triangle
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a triangle
#' triangle()
#'
#' # change the coordinates for drawing a smaller triangle
#'
#' triangle(size.x = 5)
#' }
triangle <- function(size.x=15,
                     size.y=size.x,
                     pos.x = 0,
                     pos.y = 0,
                     rot=pi / 2,
                     shd=NA,
                     vis = 1,
                     lty = 1,
                     lwd = 3) {
  value <- list(
    shape = "triangle",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(3),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a square
#'
#' Define the coordinates  of the ellipse within which a square can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param rot Rotation of the figure. Default is pi/4.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a square
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a square
#' square()
#'
#' # change the coordinates for drawing a smaller square
#'
#' square(size.x = 5)
#' }
square <- function(size.x= 15,
                   size.y= size.x,
                   rot=pi / 4,
                  pos.x = 0,
                  pos.y = 0,
                  shd=NA,
                  vis = 1,
                  lty = 1,
                  lwd = 3) {
  value <- list(
    shape = "square",
    size.x = list(size.x),
    size.y = list(size.x),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(4),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a rectangle
#'
#' Define the coordinates  of the ellipse within which a rectangle can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 8.
#' @param rot Rotation of the figure. Default is pi/4.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a rectangle
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a rectangle
#' rectangle()
#'
#' # change the coordinates for drawing a smaller rectangle
#'
#' rectangle(size.x = 5, size.y = 10)
#' }
rectangle <- function(size.x=10,
                      size.y=8,
                      rot=pi / 4,
                      pos.x = 0, pos.y = 0,
                      shd=NA,
                      lwd = 3,
                      lty = 1,
                      vis = 1) {
  value <- list(
    shape = "rectangle",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(101),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a luck
#'
#' Define the coordinates  of the ellipse within which a luck can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15.
#' @param rot Rotation of the figure. Default is pi/2.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a luck
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a luck
#' luck()
#'
#' # change the coordinates for drawing a smaller luck
#'
#' luck(size.x = 10, size.y = 15)
#' }
luck <- function(    size.x=10,
                     size.y=15,
                     rot=pi / 2,
                     pos.x = 0, pos.y = 0,
                     shd=NA,
                     vis = 1,
                     lty = 1,
                     lwd = 3) {
  value <- list(
    shape = "luck",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(4),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a pentagon
#'
#' Define the coordinates  of the ellipse within which a pentagon can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param rot Rotation of the figure. Default is pi/2.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a pentagon
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a pentagon
#' pentagon()
#'
#' # change the coordinates for drawing a smaller pentagon
#'
#' pentagon(size.x = 10,)
#' }
pentagon <- function(size.x=15,
                     size.y=size.x,
                     rot=pi / 2,
                     pos.x = 0, pos.y = 0,
                     shd=NA,
                     vis = 1,
                     lty = 1,
                     lwd = 3) {
  value <- list(
    shape = "pentagon",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(5),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a hexagon
#'
#' Define the coordinates  of the ellipse within which an hexagon can be inscribed.
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 15
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param rot Rotation of the figure. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Default is 1
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return Return the coordinates for drawing a a hexagon
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a hexagon
#' hexagon()
#'
#' # change the coordinates for drawing a smaller hexagon
#'
#' hexagon(size.x = 10,)
#' }
hexagon <- function(size.x=15,
                    size.y=size.x,
                      rot=0,
                      pos.x = 0, pos.y = 0,
                      shd=NA,
                      vis = 1,
                      lty = 1,
                    lwd = 3) {
  value <- list(
    shape = "e.hexagon",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rot),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv = list(6),
    shade = list(shd),
    visible = vis,
    tag=list(c('simple','fill', 'd.ext','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}



