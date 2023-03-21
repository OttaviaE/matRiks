#' Define the coordinates of a circle
#'
#' @param size.x Length of the x axis
#' @param size.y Length of the y axis
#' @param rot Rotation in radiants (Default is pi/4)
#' @param pos.x Position on the x axis
#' @param pos.y Position on the y axis
#' @param shd Color of the object. Deafsult is NA which results in a transparent object
#' @param rot Rotation of the ellipse in which the figure is inscribed
#' @param vis Integer, indicates whether the object should be visible (1) or not (0). Deafult is visible
#' @param lty Border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd Width of the border line. Deafult is 3.
#'
#' @return Based on the chosen shape, return an object with the information for plotting the desired design
#' @export
#'
#' @examples Ci sarà
circle <- function(size.x = 10, size.y = 10,
                   pos.x = 0, pos.y = 0, shd = NA,
                   rot = 0,
                   vis = 1,
                   lty = 1,
                   lwd = 3) {
  value <- list(
    shape = "circle",
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
    tag=list(c('simple', 'fill', 'small', 'd.ext'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of an ellipse
#'
#' @return Return the default ellipse object
#' @examples
#' ellipse()
#' @export
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
    tag=list(c('simple', 'fill', 'small','rotate', 'd.ext'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a triangle
#'
#' @return Return the default triangle object
#' @examples
#' triangle()
#' @export
triangle <- function(size.x=15,
                     size.y=15,
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
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a square
#'
#' @return Return the default square object
#' @examples
#' square()
#' @export
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
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a rectangle
#'
#' @param size.x
#' @param size.y
#' @param rot
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param lwd
#' @param lty
#' @param vis
#'
#' @return
#' @export
#'
#' @examples
rectangle <- function(size.x=15,
                      size.y=20,
                      rot=pi / 4, pos.x = 0, pos.y = 0,
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
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of a luck
#'
#' @param size.x
#' @param size.y
#' @param rot
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param vis
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
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
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of a pentagon
#'
#' @param size.x
#' @param size.y
#' @param rot
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param vis
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
pentagon <- function(size.x=15,
                     size.y=15,
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
    tag=list(c('simple', 'small','rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of an hexagon
#'
#' @param size.x
#' @param size.y
#' @param rot
#' @param pos.x
#' @param pos.y
#' @param shd
#' @param vis
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
hexagon <- function(size.x=15,
                    size.y=15,
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
    tag=list(c('simple', 'small', 'rotate'))
  )
  attr(value, "class") <- "figure"
  value
}



