#' Arches of a circle
#'
#' @param size.x
#' @param size.y
#' @param lty
#' @param lwd
#' @param vis
#' @param pos.x
#' @param pos.y
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
  attr(value, "class") <- "cell"
  value
}

#' Vertical arc right up
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
  attr(value, "class") <- "cell"
  value
}


#' Vertical arc left down
#'
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
  attr(value, "class") <- "cell"
  value
}


#' Vertical arc right down
#'
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
  attr(value, "class") <- "cell"
  value
}


#' Horizontal arc left up
#'
#' @return Return the horizontal arc left up object
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
  attr(value, "class") <- "cell"
  value
}

#' Horizontal arc right up
#'
#' @return Return the horizontal arc right up object
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
  attr(value, "class") <- "cell"
  value
}


#' Horizontal arc left down
#'
#' @return Return the horizontal arc left down object
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
  attr(value, "class") <- "cell"
  value
}

#' Horizontal arc right down
#'
#' @return Return the horizontal arc right down object
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
  attr(value, "class") <- "cell"
  value
}
