#' Coordinates of a circle
#'
#' Define the coordinates for drawing a circle
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 10
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x
#' @param pos.x numeric, position on the x axis. Default is 0.
#' @param pos.y numeric, position the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is \eqn{\frac{\pi}{4}}
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is  \eqn{\frac{3\pi}{4}}
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0
#' @param shd character, define the shading of the figure. Default is NA which results in a transparent figure
#'
#' @return Return the coordinates for drawing a circle
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a circle
#' circle()
#' # change the coordinates for drawing a smaller circle
#' circle(size.x = 5)
circle <- function(size.x = 10, size.y = size.x,
                   pos.x = 0, pos.y = 0,
                   lty = 1,  lwd = 3,
                   shd = NA, vis = 1) {
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
#' Define the coordinates for drawing an ellipse
#'
#' @inheritParams circle
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 7
#' @param rot define the rotation. Default is 0
#'
#' @return Return the coordinates for drawing a ellipse
#' @export
#'
#' @examples
#' # return the default coordinates for drawing an ellipse
#' ellipse()
#' # change the coordinates for drawing a smaller ellipse
#' ellipse(size.x = 5, size.y = 3)
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
#' Define the coordinates for drawing a triangle
#'
#' @inheritParams circle
#' @param rot define the rotation. Default is \eqn{\frac{\pi}{2}}
#'
#' @return Return the coordinates for drawing a triangle
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a triangle
#' triangle()
#' # change the coordinates for drawing a smaller triangle
#' triangle(size.x = 5)
triangle <- function(size.x=10,
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
#' Define the coordinates for drawing a square of the ellipse within which a square can be inscribed.
#'
#' @inheritParams circle
#' @param rot define the rotation. Default is \eqn{\frac{pi}{4}}
#'
#' @return Return the coordinates for drawing a square
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a square
#' square()
#' # change the coordinates for drawing a smaller square
#' square(size.x = 5)
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



#' Coordinates of a luck
#'
#' Define the coordinates  for drawing a luck of the ellipse within which a luck can be inscribed.
#'
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 15
#' @param rot define the rotation. Default is \eqn{\frac{pi}{2}}
#'
#' @inheritParams circle
#'
#' @return Return the coordinates for drawing a luck
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a luck
#' luck()
#' # change the coordinates for drawing a smaller luck
#' luck(size.x = 10, size.y = 15)
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
#' Define the coordinates for drawing a pentagon
#'
#' @param size.x numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 15
#' @inheritParams circle
#' @param rot define the rotation. Default is \eqn{\frac{pi}{2}}
#'
#' @return Return the coordinates for drawing a pentagon
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a pentagon
#' pentagon()
#' # change the coordinates for drawing a smaller pentagon
#' pentagon(size.x = 10)
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
#' Define the coordinates for drawing an hexagon
#'
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 15
#' @inheritParams circle
#' @param rot define the rotation. Default is 0
#'
#' @return Return the coordinates for drawing an hexagon
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a hexagon
#' hexagon()
#' # change the coordinates for drawing a smaller hexagon
#' hexagon(size.x = 10)
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



