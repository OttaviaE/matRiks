#' Coordinates of a pizza slice
#'
#' Define the coordinates of a circle section
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is 3*pi/4.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @return The coordinates of a circle section
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the pizza slice
#' slice()
#' # change the rotation of the pizza slice
#' slice(theta1 = 3*pi/4, theta2 = 5*pi/4)
#' }
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
    tag = list("simple", "fill", "rotate", "d.int")
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a pacman
#'
#' Define the coordinates of the circle sections drawing a pacman
#'
#' @param size.x The length of the x-axis. Default is sqrt(square()$ size.x[[1]]^2 /2).
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is pi/4.
#' @param theta2 Ending angle of the circle section. Default is 7*pi/4.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of a pacman
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of pacman
#' slice()
#' # draw an actual pacman
#' draw(cof(pacman(shd = "yellow"), dot(pos.y = 6)))
#' }
pacman <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                   size.y = 0,pos.x=0 ,pos.y=0,
                   theta1 = pi/4,
                   theta2 = 7*pi/4,
                   lty = 1, lwd =3,shd=NA,
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
    tag = list("simple","fill", "rotate", "d.int" )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a pizza with four slices
#'
#' Define the coordinates of the circle section composing a pizza with four slices
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of four circle sections composing a pizza with four slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the pizza with four slices
#' pizza.4()
#' }
pizza.4 <- function(size.x = 15, shd = NA, lwd = 3, lty =1) {
  value <-cof(slice(size.x = size.x, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 3*pi/4, theta2 = 5*pi/4, shd = shd,  lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 5*pi/4, theta2 = 7*pi/4, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 7*pi/4, theta2 = 9*pi/4, shd = shd, lty = lty, lwd = lwd))
  value$tag <- list("compose4","fill", "d.ext")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single pizza with four slices
#'
#' Define the coordinates of the circle section composing a pizza with four slices, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of four circle sections composing a single pizza with four slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single pizza with four slices
#' s.pizza.4()
#' }
s.pizza.4 <- function(size.x = 15, shd = NA, lwd = 3, lty =1) {
  value <-cof(slice(size.x = size.x, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 3*pi/4, theta2 = 5*pi/4, shd = shd,  lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 5*pi/4, theta2 = 7*pi/4, shd = shd, lty = lty, lwd = lwd),
              slice(size.x = size.x,theta1 = 7*pi/4, theta2 = 9*pi/4, shd = shd, lty = lty, lwd = lwd),
              name = "s.pizza.4",
              single = TRUE)
  value$tag <- list("simple","fill", "d.int")
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a downward-facing left semi-circle
#'
#' Define the coordinates of a downward-facing left semi-circle
#'
#' @param size.x The length of the x-axis. Default is sqrt(square()$ size.x[[1]]^2 /2).
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is 5*pi/4.
#' @param shd Color of the figure Default is NA which results in a transparent figure.
#' @param lwd The line width. Default is 3.
#' @param lty The line type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return The coordinates a downward-facing left semi-circle
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the downward-facing left semi-circle
#' semi.circle()
#' # change the rotation of the downward-facing left semi-circle
#' semi.circle(theta1 = pi/2, theta2 = 3*pi/2)
#' }
semi.circle <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                        size.y = 0,
                        pos.x=0 ,pos.y=0,
                        theta1 = pi/4,
                        theta2 = 5*pi/4,
                        shd = NA,
                        lty = 1,
                        lwd =3,
                        vis = 1) {
  value <- list(
    shape = "semi.circle",
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
    tag = list("simple","fill", "rotate" )
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a upward-facing left semi-circle
#'
#' Define the coordinates of a upward-facing left semi-circle
#'
#' @param size.x The length of the x-axis. Default is sqrt(square()$ size.x[[1]]^2 /2).
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is 5*pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is pi/4.
#' @param shd Color of the figure Default is NA which results in a transparent figure.
#' @param lwd The line width. Default is 3.
#' @param lty The line type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return The coordinates a upward-facing left semi-circle
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the upward-facing left semi-circle
#' semi.circle.inv()
#' # change the rotation of the upward-facing left semi-circle
#' semi.circle.inv(theta1 = pi, theta2 = 2*pi)
#' }
semi.circle.inv <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                        size.y = 0,
                        pos.x=0 ,pos.y=0,
                        theta1 = 5*pi/4,
                        theta2 = pi/4,
                        shd = NA,
                        lty = 1,
                        lwd =3,
                        vis = 1) {
  value <- list(
    shape = "semi.circle.inv",
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
    tag = list("simple","fill", "rotate" )
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a pizza with two slices
#'
#' Define the coordinates of the circle section composing a pizza with two slices
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of two circle sections composing a pizza with two slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the pizza with two slices
#' pizza.2()
#' }
pizza.2 <- function(size.x = 15, size.y = 0,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi.circle(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = NA,
                          lty = lty,
                          lwd = lwd),
              semi.circle.inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = NA,
                              lty = lty,
                              lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single pizza with two slices
#'
#' Define the coordinates of the circle section composing a pizza with two slices, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of two circle sections composing a single pizza with two slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single pizza with two slices
#' s.pizza.2()
#' }
s.pizza.2 <- function(size.x = 15, size.y = 0,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi.circle(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = NA,
                          lty = lty,
                          lwd = lwd),
              semi.circle.inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = NA,
                              lty = lty,
                              lwd = lwd),
              name= "s.pizza.2",
              single = T)
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a downward-facing right semi-circle
#'
#' Define the coordinates of a downward-facing right semi-circle
#'
#' @param size.x The length of the x-axis. Default is sqrt(square()$ size.x[[1]]^2 /2).
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is 7*pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is 3*pi/4.
#' @param shd Color of the figure Default is NA which results in a transparent figure.
#' @param lwd The line width. Default is 3.
#' @param lty The line type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return The coordinates a downward-facing right semi-circle
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the downward-facing right semi-circle
#' semi.circle.rev()
#' # change the rotation of the downward-facing right semi-circle
#' semi.circle.rev(theta1 = 0, theta2 = pi/2)
#' }
semi.circle.rev <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                        size.y = 0,
                        pos.x=0 ,pos.y=0,
                        theta1 = 7*pi/4,
                        theta2 = 3*pi/4,
                        shd = NA,
                        rotation = pi - pi / 4,
                        lty = 1,
                        lwd =3,
                        vis = 1) {
  value <- list(
    shape = "semi.circle.rev",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(rotation),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list("simple","fill", "rotate" )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a upward-facing left semi-circle
#'
#' Define the coordinates of a upward-facing left semi-circle
#'
#' @param size.x The length of the x-axis. Default is sqrt(square()$ size.x[[1]]^2 /2).
#' @param size.y The length of the x-axis. Default is 0.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param theta1 Starting angle of the circle section. Default is 3*pi/4.
#' @param theta2 Ending angle of the circle section (built counterclockwise). Default is 7*pi/4.
#' @param shd Color of the figure Default is NA which results in a transparent figure.
#' @param lwd The line width. Default is 3.
#' @param lty The line type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return The coordinates a upward-facing left semi-circle
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the upward-facing left semi-circle
#' semi.circle.rev.inv()
#' # change the rotation of the upward-facing left semi-circle
#' semi.circle.rev.inv(theta1 = pi, theta2 = 2*pi)
#' }
semi.circle.rev.inv <- function(size.x =sqrt(square()$ size.x[[1]]^2 /2),
                            size.y = 0,
                            pos.x=0 ,pos.y=0,
                            theta1 = 3*pi/4,
                            theta2 = 7*pi/4,
                            shd = NA,
                            rotation = pi - pi / 4,
                            lty = 1,
                            lwd =3,
                            vis = 1) {
  value <- list(
    shape = "semi.circle.inv",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(theta1),
    theta.2  = list(theta2),
    rotation = list(rotation),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(2),
    nv =  list(100),
    shade = list(shd),
    visible = vis,
    tag = list("simple","fill", "rotate" )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an inverse pizza with two slices
#'
#' Define the coordinates of the circle section composing an inverse pizza with two slices
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of two circle sections composing an inverse pizza with two slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single inverse pizza with two slices
#' pizza.2.rev()
#' }
pizza.2.rev <- function(size.x = 15, size.y = 0,
                  pos.x=0 ,pos.y=0,
                  shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi.circle.rev(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = NA,
                          lty = lty,
                          lwd = lwd),
              semi.circle.rev.inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = NA,
                              lty = lty,
                              lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single inverse pizza with two slices
#'
#' Define the coordinates of the circle section composing an inverse pizza with two slices, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x The length of the x-axis. Default is 15.
#' @param lwd The line width. Default is 3.
#' @param lty The lime type, default is 1 (solid line).
#' @param shd Color of the figure Default is NA which results in a transparent figure
#'
#' @return The coordinates of two circle sections composing a single pizza with two slices
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single inverse pizza with two slices
#' s.pizza.2()
#' }
s.pizza.2.rev <- function(size.x = 15, size.y = 0,
                    pos.x=0 ,pos.y=0,
                    shd = NA, lty = 1, lwd = 3) {
  value <-cof(semi.circle(size.x = size.x,
                          size.y = size.y,
                          pos.x = pos.x,
                          pos.y = pos.y,
                          shd = NA,
                          lty = lty,
                          lwd = lwd),
              semi.circle.inv(size.x = size.x,
                              size.y = size.y,
                              pos.x = pos.x,
                              pos.y = pos.y,
                              shd = NA,
                              lty = lty,
                              lwd = lwd),
              name= "s.pizza.2.rev",
              single = T)
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "figure"
  value
}

