#' Coordinates of a vertical left up arch
#'
#' Define the coordinates of the left up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the left up arch
#'
#' v.arc.left.up()
#'
#' }
v.arc.left.up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a vertical right up arch
#'
#' Define the coordinates of the right up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the right up arch
#'
#' v.arc.right.up()
#'
#' }
v.arc.right.up <- function(size.x=square()$size.x[[1]]/2,
                           size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a vertical left down arch
#'
#' Define the coordinates of the left down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the left down arch
#'
#' v.arc.left.down()
#'
#' }
v.arc.left.down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a vertical right down arch
#'
#' Define the coordinates of the right down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the right down arch
#'
#' v.arc.right.down()
#'
#' }
v.arc.right.down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a horizontal left up arch
#'
#' Define the coordinates of the left up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the left up arch
#'
#' h.arc.left.up()
#'
#' }
h.arc.left.up <- function(size.x=square()$size.x[[1]]/2,
                          size.y=size.x,
                          lty =1, lwd = 3, vis = 1, pos.x = 0, pos.y = 0) {
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a horizontal right up arch
#'
#' Define the coordinates of the right up arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the right up arch
#'
#' h.arc.right.up()
#'
#' }
h.arc.right.up <- function(size.x=square()$size.x[[1]]/2,
                           size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a horizontal left down arch
#'
#' Define the coordinates of the left down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the left down arch
#'
#' h.arc.left.down()
#'
#' }
h.arc.left.down <- function(size.x=square()$size.x[[1]]/2,
                            size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a horizontal right down arch
#'
#' Define the coordinates of the right down arch of a circle
#'
#' @param size.x The length of the x-axis. Default is square()$size.x[[1]]/2.
#' @param size.y The length of the x-axis. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param pos.y Position on the y axis, Default is 0.
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#' @param vis Visibility of the figure. Deafult is 1, making the figure visible. To hide the figure, change it to 0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the right down arch
#'
#' h.arc.right.down()
#'
#' }
h.arc.right.down <- function(size.x=square()$size.x[[1]]/2,
                             size.y=size.x,
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
    tag=list(c('simple', 'no.d'))
  )
  attr(value, "class") <- "figure"
  value
}
