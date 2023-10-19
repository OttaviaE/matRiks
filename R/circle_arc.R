#' Coordinates for drawing circle arches
#'
#' Define the coordinates for drawing different circle arches
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2
#' @param size.y The length of the x-axis. Default is size.x
#' @param pos.x Position on the x axis. Default is 0
#' @param pos.y Position on the y axis, Default is 0
#' @param lwd The line width. Default is 3
#' @param lty The line type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the left up arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the left up arch
#' v_arc_left_up()
v_arc_left_up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=size.x,
                          pos.x = 0, pos.y = 0,
                          vis = 1,
                          lty =1, lwd = 3) {
  value <- list(
    shape = "v_arc_left_up",
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
    nv  = list(100),
    shade  = list(NA),
    visible = vis,
    tag=list(c("simple", "no.d", "vert"))
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn v_arc_left_up Coordinates of a vertical right up arch
#'
#' Define the coordinates for drawing the right up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the right up arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the right up arch
#' v_arc_right_up()

v_arc_right_up <- function(size.x=square()$size.x[[1]]/2, size.y=size.x, pos.x = 0, pos.y = 0, lty =1, lwd = 3,
                           vis = 1) {
  value <- list(
    shape = "v_arc_right_up",
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
    tag=list(c('simple', 'no.d', "vert"))
  )
  attr(value, "class") <- "figure"
  value
}


#' @describeIn v_arc_left_up Coordinates of a vertical left down arch
#'
#' Define the coordinates for drawing the left down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2
#' @param size.y The length of the x-axis. Default is size.x
#' @param pos.x Position on the x axis. Default is 0
#' @param pos.y Position on the y axis, Default is 0
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the left down arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the left down arch
#' v_arc_left_down()
v_arc_left_down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=size.x,
                            lty =1, lwd = 3, vis  = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "v_arc_left_down",
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
    tag=list(c('simple', 'no.d', "vert"))
  )
  attr(value, "class") <- "figure"
  value
}


#' @describeIn v_arc_left_up Coordinates of a vertical right down arch
#'
#' Define the coordinates for drawing f the right down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the right down arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the right down arch
#' v_arc_right_down()
v_arc_right_down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=size.x,
                             lty =1, lwd = 3,vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "v_arc_right_down",
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
    tag=list(c('simple', 'no.d', "vert"))
  )
  attr(value, "class") <- "figure"
  value
}


#' @describeIn v_arc_left_up Coordinates of a horizontal left up arch
#'
#' Define the coordinates for drawing the left up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the left up arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the left up arch
#' h_arc_left_up()
h_arc_left_up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=size.x,
                          lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "h_arc_left_up",
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
    tag=list(c('simple', 'no.d', "vert"))
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn v_arc_left_up Coordinates of a horizontal right up arch
#'
#' Define the coordinates for drawing the right up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the right up arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the right up arch
#' h_arc_right_up()
h_arc_right_up <- function(size.x=square()$size.x[[1]]/2,
                           size.y=size.x,
                           lty =1, lwd = 3,vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape  = "h_arc_right_up",
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
    tag=list(c('simple', 'no.d', "hor"))
  )
  attr(value, "class") <- "figure"
  value
}


#' @describeIn v_arc_left_up Coordinates of a horizontal left down arch
#'
#' Define the coordinates for drawing the left down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the left down arch of a circle
#' @export
#'
#' @examples
#' # default coordinates of the left down arch
#' h_arc_left_down()
h_arc_left_down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=size.x,
                            lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "h_arc_left_down",
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
    nv  = list(100),
    shade  = list(NA),
    visible = vis,
    tag=list(c('simple', 'no.d', "hor"))
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn v_arc_left_up Coordinates of a horizontal right down arch
#'
#' Define the coordinates for drawing the right down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Default is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return Return the coordinates for drawing the right down arch
#' @export
#'
#' @examples
#' # default coordinates of the right down arch
#' h_arc_right_down()
h_arc_right_down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=size.x,
                             lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
  value <- list(
    shape = "h_arc_right_down",
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
    tag=list(c('simple', 'no.d', "hor"))
  )
  attr(value, "class") <- "figure"
  value
}
