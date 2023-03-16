#' Define the coordinates for drawing the vertical left up arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
v.arc.left.up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=square()$size.y[[1]]/2,
                          pos.x = 0, pos.y = 0,
                          vis = 1,
                          lty =1, lwd = 3) {
  value <- list(
    shape = "v.arc.left.up",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(3*pi/4),
    theta.2  = list(5*pi/4),
    rotation = list(pi),
    pos.x  = list(5),
    pos.y  = list(5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing the vertical right up arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the vertical arc right up object
#' @examples
#' v.arc.right.up()
#' @export
v.arc.right.up <- function(size.x=square()$size.x[[1]]/2,
                           size.y=square()$size.y[[1]]/2,
                           pos.x = 0, pos.y = 0,
                           vis = 1,
                           lty =1, lwd = 3) {
  value <- list(
    shape = "v.arc.right.up",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(7*pi/4),
    theta.2  = list(pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates for drawing the vertical left down arch
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#' @return Return the vertical arc left down object
#' @examples
#' v.arc.left.down()
#' @export
v.arc.left.down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=square()$size.y[[1]]/2,
                            lty =1, lwd = 3, vis  = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "v.arc.left.down",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(3*pi/4),
    theta.2  = list(5*pi/4),
    rotation = list(pi),
    pos.x  = list(5.1),
    pos.y  = list(-5.5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of the vertical right down arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#' @return Return the vertical arc right down object
#' @examples
#' v.arc.right.down()
#' @export
v.arc.right.down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=square()$size.y[[1]]/2,
                             lty =1, lwd = 3,vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "v.arc.right.down",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(7*pi/4),
    theta.2  = list(pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(-5.5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of the horizontal left up arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the horizontal arc left up object
#'
#' @examples
#' h.arc.left.up()
#' @export
h.arc.left.up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=square()$size.y[[1]]/2,lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "h.arc.left.up",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(pi/4),
    theta.2  = list(3*pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(-5.5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100),
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of the horizontal right up arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the horizontal arc right up object
#'
#' @examples
#' h.arc.right.up()
#' @export
h.arc.right.up <- function(size.x=square()$size.x[[1]]/2,
                           size.y=square()$size.y[[1]]/2,
                           lty =1, lwd = 3,vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "h.arc.right.up",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(pi/4),
    theta.2  = list(3*pi/4),
    rotation = list(pi),
    pos.x  = list(5.1),
    pos.y  = list(-5.5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100),
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of the horizontal left down arch
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the horizontal arc left down object
#'
#' @examples
#' h.arc.left.down()
#' @export
h.arc.left.down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=square()$size.y[[1]]/2,
                            lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "h.arc.left.down",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x  = list(-5.5),
    pos.y  = list(5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of the horizontal right down arch
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is square()$size.y[[1]]/2.
#' @param pos.x The x coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param pos.y The y coordinate on the Cartesian system with origin (0, 0). Deafult is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the horizontal arc right down object
#'
#' @examples
#' h.arc.right.down()
#' @export
h.arc.right.down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=square()$size.y[[1]]/2,
                             lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "h.arc.right.down",
    size.x  = list(size.x),
    size.y  = list(size.y),
    theta.1  = list(5*pi/4),
    theta.2  = list(7*pi/4),
    rotation = list(pi),
    pos.x  = list(5),
    pos.y  = list(5),
    lty  = list(lty),
    lwd  = list(lwd),
    num  = list(2),
    nv  = list(100), # non cambia nulla che sia indicato o meno
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'small'))
  )
  attr(value, "class") <- "figure"
  value
}
