#' Define the coordinates for drawing a lily
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
lily <- function(lwd = 3, lty = 1) {
  value <-cof( horizontal.eight(lwd = lwd, lty = 1),
               vertical.eight(lwd = lwd, lty = 1))
  value$tag <- list("compose4")
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates for drawing a single lily (to be used in `shape()`)
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
s.lily <- function(lwd = 3, lty = 1) {
  value <-cof( s.horizontal.eight(lwd = lwd, lty = 1),
               s.vertical.eight(lwd = lwd, lty = 1),
               name = "s.lily",
               single = T)
  value$tag <- list("compose4")
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates for drawing the up petal
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
up.petal = function(lwd = 3, lty = 1) {
 value =  cof(v.arc.left.up(lwd = lwd, lty = lty),
         v.arc.right.up(lwd = lwd, lty = lty),
        name="up.petal",
        single = T)
 value$tag = list("compose2")
 attr(value, "class") <- "figure"
 value
}


#' Define the coordinates for drawing the down petal
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
down.petal = function(lwd = 3, lty = 1) {
  value =  cof(v.arc.left.down(lwd = lwd, lty = lty),
               v.arc.right.down(lwd = lwd, lty = lty),
               name="down.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates for drawing the left petal
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
left.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.left.down(lwd = lwd, lty = lty),
               h.arc.left.up(lwd = lwd, lty = lty),
               name="left.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates for drawing the right petal
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
right.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.right.down(lwd = lwd, lty = lty),
               h.arc.right.up(lwd = lwd, lty = lty),
               name="right.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates for drawing a flower
#'
#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down.petal(lwd = lwd, lty = lty),
              left.petal(lwd = lwd, lty = lty),
              right.petal(lwd = lwd, lty = lty))
  value$tag = list("compose4")
  attr(value, "class") = "figure"
  value
}
#' Define the coordinates for drawing a single flower (to be used in `shape()`)
#'

#' @param lwd The line width. Default is 3
#' @param lty The lime type, default is 1 (solid line).
#'
#' @return
#' @export
#'
#' @examples
s.flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down.petal(lwd = lwd, lty = lty),
              left.petal(lwd = lwd, lty = lty),
              right.petal(lwd = lwd, lty = lty),
              name = "flower",
              single = TRUE)
  value$tag = list("compose4")
  attr(value, "class") = "figure"
  value
}


