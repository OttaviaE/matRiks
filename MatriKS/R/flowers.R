#' Define the coordinates of a a lily
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
lily <- function(lwd = 3, lty = 1) {
  value <-cof( horizontal.eight(lwd = lwd, lty = 1),
               vertical.eight(lwd = lwd, lty = 1))
  value$tag <- list("compose4")
  attr(value, "class") <- "cell"
  value
}


#' Define the coordinates of a a single lily (to be used in diff_shapes)
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.lily <- function(lwd = 3, lty = 1) {
  value <-cof( horizontal.eight(lwd = lwd, lty = 1),
               vertical.eight(lwd = lwd, lty = 1),
               name = "s.lily",
               single = T)
  value$tag <- list("compose4")
  attr(value, "class") <- "cell"
  value
}


#' Define the coordinates of a the up petal
#'
#' @param lwd
#' @param lty
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
 attr(value, "class") <- "cell"
 value
}


#' Define the coordinates of a the down petal
#'
#' @param lwd
#' @param lty
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
  attr(value, "class") <- "cell"
  value
}

#' Define the coordinates of a the left petal
#'
#' @param lwd
#' @param lty
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
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a the right petal
#'
#' @param lwd
#' @param lty
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
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a a flower
#'
#' @param lwd
#' @param lty
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
  attr(value, "class") = "cell"
  value
}
#' Define the coordinates of a single flower (to be used in diff shapes)
#'
#' @param lwd
#' @param lty
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
  attr(value, "class") = "cell"
  value
}


