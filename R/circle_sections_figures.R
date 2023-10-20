#' Coordinates of a pizza slice
#'
#' Define the coordinates for drawing a circle section
#'
#' @param size.x integer, length of the semi-major axis of the ellipse within which the figure is inscribed. Default is 15
#' @param size.y integer, length of the semi-major axis of the ellipse within which the figure is inscribed. Default is 0
#' @param pos.x numeric, position on the x axis. Default is 0
#' @param pos.y numeric, position the y axis, Default is 0
#' @param theta1 Starting angle of the circle section. Default is \eqn{\frac{\pi}{4}}
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is  \eqn{\frac{3\pi}{4}}
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0
#' @param shd character, define the shading of the figure. Default is NA which results in a transparent figure
#'
#' @return Return the coordinates for drawing a circle section
#'
#' @export
#'
#' @examples
#' # default coordinates of the pizza slice
#' slice()
#' # change the rotation of the pizza slice
#' slice(theta1 = 3*pi/4, theta2 = 5*pi/4)
slice <- function(size.x =15,
                  size.y = 0,
                  pos.x=0,
                  pos.y=0,
                  theta1 = pi/4,
                  theta2 = 3*pi/4,
                  lty = 1, lwd =3,
                  vis = 1,
                  shd = NA) {
  value <- list(
    shape = "slice",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(pi - pi / 4),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple", "fill", "rotate", "d.int"))
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a pacman
#'
#' Define the coordinates for drawing the circle sections for drawing a pacman
#'
#' @param size.x integer, length of the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y integer, length of the semi-mino axis of the ellipse within which the figure is inscribed. Default is 0
#' @param theta1 Starting angle of the circle section. Default is  \eqn{\frac{\pi}{4}}
#' @param theta2 Ending angle of the circle section. Default is  \eqn{\frac{7\pi}{4}}
#' @inheritParams slice
#'
#' @return Return the coordinates for drawing a pacman
#' @export
#'
#' @examples
#' # default coordinates of pacman
#' slice()
#' # draw an actual pacman
#' draw(cof(pacman(shd = "yellow"), dot(pos.y = 6)))
pacman <- function(size.x =sqrt(square()$size.x[[1]]^2 /2), size.y = 0,
                   pos.x=0 ,pos.y=0,
                   theta1 = pi/4, theta2 = 7*pi/4,
                   lty = 1, lwd =3,
                   shd=NA,
                   vis = 1) {
  value <- list(
    shape = "pacman",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(pi - pi / 4),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple","fill", "rotate", "d.int" ))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a pizza with four slices
#'
#' Define the coordinates for drawing the circle sections composing a pizza with four slices
#'
#' @param size.x An integer giving the semi-major axis of the ellipse  within which the figure is inscribed. Default is 15
#' @inheritParams slice
#'
#' @return Return the coordinates for drawing four circle sections composing a pizza with four slices
#' @export
#'
#' @examples
#' # default coordinates of the pizza with four slices
#' pizza_4()
pizza_4 <- function(size.x = 15, shd = NA, lwd = 3, lty =1) {
  value <-cof(slice(size.x = size.x,theta1 = 5*pi/4, theta2 = 7*pi/4, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 3*pi/4, theta2 = 5*pi/4, shd = shd,  lty = lty, lwd = lwd),
              slice(size.x = size.x, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 7*pi/4, theta2 = 9*pi/4, shd = shd, lty = lty, lwd = lwd))
  value$tag <- list(c("compose4","fill", "d.ext", "rotate"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn pizza_4 Coordinates of a single pizza with four slices
#'
#' Define the coordinates for drawing the circle section composing a single pizza with four slices, to be used in shape()
#'
#' @inheritParams pizza_4
#'
#'
#' @return Return the coordinates for drawing four circle sections composing a singledocu pizza with four slices
#' @export
#'
#' @examples
#' # default coordinates of the single pizza with four slices
#' s_pizza_4()
s_pizza_4 <- function(size.x = 15, shd = NA, lwd = 3, lty =1) {
  value <-cof(slice(size.x = size.x,theta1 = 5*pi/4, theta2 = 7*pi/4, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 3*pi/4, theta2 = 5*pi/4, shd = shd,  lty = lty, lwd = lwd),
              slice(size.x = size.x, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 7*pi/4, theta2 = 9*pi/4, shd = shd, lty = lty, lwd = lwd),
              name = "s_pizza_4",
              single = TRUE)
  value$tag <- list(c("simple","fill", "d.int", "rotate"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a downward-facing left semi-circle
#'
#' Define the coordinates for drawing a downward-facing left semi-circle
#'
#' @param size.x numeric, define the semi-major axis of the ellipse  within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse  within which the figure is inscribed. Default is 0
#' @param theta1 Starting angle of the circle section. Default is \eqn{\frac{\pi}{4}}
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is \eqn{\frac{5\pi}{4}}
#' @inheritParams slice
#'
#' @return Return the coordinates for drawing downward-facing left semi-circle
#' @export
#'
#' @examples
#' # default coordinates of the downward-facing left semi-circle
#' semi_circle_top()
#' # change the rotation of the downward-facing left semi-circle
#' semi_circle_top(theta1 = pi/2, theta2 = 3*pi/2)
semi_circle_top <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2), size.y = 0,
                        pos.x=0 ,pos.y=0,
                        theta1 = pi/4, theta2 = 5*pi/4,
                        lty = 1, lwd =3,
                        shd = NA,
                        vis = 1) {
  value <- list(
    shape = "semi_circle_top",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(0),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple","fill", "rotate") )
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of an upward-facing right semi-circle
#'
#' Define the coordinates fr drawing an upward-facing right semi-circle
#'
#' @inheritParams semi_circle_top
#' @param theta1 Starting angle of the circle section. Default is \eqn{\frac{5\pi}{4}}
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is \eqn{\frac{\pi}{4}}
#'
#' @return The coordinates for drawing an upward-facing left semi-circle
#' @export
#'
#' @examples
#' # default coordinates of the upward-facing right semi-circle
#' semi_circle_bottom_inv()
#' # change the rotation of the upward-facing right semi-circle
#' semi_circle_bottom_inv(theta1 = pi, theta2 = 2*pi)
semi_circle_bottom_inv <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                                   size.y = 0,
                                   pos.x=0 ,pos.y=0,
                                   theta1 = 5*pi/4,
                                   theta2 = pi/4,
                                   shd = NA,
                                   lty = 1,
                                   lwd =3,
                                   vis = 1) {
  value <- list(
    shape = "semi_circle_bottom_inv",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(0),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple","fill", "rotate") )
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a pizza with two slices
#'
#' Define the coordinates for drawing the circle sections composing a pizza with two slices
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 15
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 0
#' @inheritParams slice
#'
#' @return Return the coordinates for drawing two circle sections composing a pizza with two slices
#' @export
#'
#' @examples
#' # default coordinates of the pizza with two slices
#' pizza_2()
pizza_2 <- function(size.x = 15, size.y = 0,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi_circle_top(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = shd,
                          lty = lty,
                          lwd = lwd),
              semi_circle_bottom_inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = shd,
                              lty = lty,
                              lwd = lwd))
  value$tag <- list(c("compose2","fill", "rotate"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn pizza_2 Coordinates of a single pizza with two slices
#'
#' Define the coordinates for drawing the circle section composing a single pizza with two slices, to be used in shape()
#'
#' @inheritParams pizza_2
#'
#'
#' @return Return the coordinates for drawing two circle sections composing a single pizza with two slices
#' @export
#'
#' @examples
#' # default coordinates of the single pizza with two slices
#' s_pizza_2()
s_pizza_2 <- function(size.x = 15, size.y = 0,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi_circle_top(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = shd,
                              lty = lty,
                              lwd = lwd),
              semi_circle_bottom_inv(size.x = size.x,
                                     size.y = size.y,
                                     pos.x = pos.x,
                                     pos.y = pos.y,
                                     shd = shd,
                                     lty = lty,
                                     lwd = lwd),
              name= "s_pizza_2",
              single = T)
  value$tag <- list(c("compose2","fill", "rotate"))
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a downward-facing right semi-circle
#'
#' Define the coordinates for drawing a downward-facing right semi-circle
#'
#' @param theta1 Starting angle of the circle section. Default is \eqn{\frac{7\pi}{4}}
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is \eqn{\frac{3\pi}{4}}.
#' @inheritParams semi_circle_top
#'
#' @return Return the coordinates for drawing a downward-facing right semi-circle
#' @export
#'
#' @examples
#' # default coordinates of the downward-facing right semi-circle
#' semi_circle_top_inv()
#' # change the rotation of the downward-facing right semi-circle
#' semi_circle_top_inv(theta1 = 0, theta2 = pi/2)
semi_circle_top_inv <- function(size.x =sqrt(square()$size.x[[1]]^2 /2), size.y = 0,
                                pos.x=0 ,pos.y=0,
                                theta1 = 7*pi/4,
                                theta2 = 3*pi/4,
                                shd = NA,
                                lty = 1,
                                lwd =3,
                                vis = 1) {
  value <- list(
    shape = "semi_circle_top_inv",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(0),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple","fill", "rotate") )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an upward-facing left semi-circle
#'
#' Define the coordinates for drawing an upward-facing left semi-circle
#'
#' @param theta1 Starting angle of the circle section. Default is 3*pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is 7*pi/4.
#' @inheritParams semi_circle_top_inv
#'
#' @return The coordinates a upward-facing left semi-circle
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the upward-facing left semi-circle
#' semi_circle_inv_inv()
#' # change the rotation of the upward-facing left semi-circle
#' semi_circle_inv_inv(theta1 = pi, theta2 = 2*pi)
#' }
semi_circle_bottom <- function(size.x =sqrt(square()$size.x[[1]]^2 /2),
                               size.y = 0,
                               pos.x=0 ,pos.y=0,
                               theta1 = 3*pi/4,
                               theta2 = 7*pi/4,
                               shd = NA,
                               lty = 1,
                               lwd =3,
                               vis = 1) {
  value <- list(
    shape = "semi_circle_bottom",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(0),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list(c("simple","fill", "rotate") )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an inverse pizza with two slices
#'
#' Define the coordinates for drawing the circle sections composing an inverse pizza with two slices
#'
#' @inheritParams pizza_2
#'
#' @return The coordinates of two circle sections composing an inverse pizza with two slices
#' @export
#'
#' @examples
#' # default coordinates of the inverse pizza with two slices
#' pizza_2_inv()
pizza_2_inv <- function(size.x = 15, size.y = size.x,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi_circle_bottom(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = shd,
                          lty = lty,
                          lwd = lwd),
              semi_circle_top_inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = shd,
                              lty = lty,
                              lwd = lwd))
  value$tag <- list(c("compose2","fill", "rotate"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn pizza_2_inv Coordinates of a single inverse pizza with two slices
#'
#' Define the coordinates for drawing the circle sections composing an inverse pizza with two slices, to be used in shape()
#'
#' @inheritParams pizza_2_inv
#'
#' @return The coordinates of two circle sections composing a single pizza with two slices
#' @export
#'
#' @examples
#' # default coordinates of the single inverse pizza with two slices
#' s_pizza_2()
s_pizza_2_inv <- function(size.x = 15, size.y = 0,
                    pos.x=0 ,pos.y=0,
                    shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi_circle_bottom(size.x = size.x,
                                 size.y = size.y,
                                 pos.x = pos.x,
                                 pos.y = pos.y,
                                 shd = shd,
                                 lty = lty,
                                 lwd = lwd),
              semi_circle_top_inv(size.x = size.x,
                                  size.y = size.y,
                                  pos.x = pos.x,
                                  pos.y = pos.y,
                                  shd = shd,
                                  lty = lty,
                                  lwd = lwd),
              name= "s_pizza_2_inv",
              single = T)
  value$tag <- list(c("compose2","fill", "rotate"))
  attr(value, "class") <- "figure"
  value
}

