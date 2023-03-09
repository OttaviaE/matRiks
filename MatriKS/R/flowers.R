#' Flowers
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
s.lily <- function(lwd = 3, lty = 1) {
  value <-cof( horizontal.eight(lwd = lwd, lty = 1),
               vertical.eight(lwd = lwd, lty = 1),
               name = "s.lily",
               single = T)
  value$tag <- list("compose4")
  attr(value, "class") <- "cell"
  value
}

up.petal = function(lwd = 3, lty = 1) {
 value =  cof(v.arc.left.up(lwd = lwd, lty = lty),
         v.arc.right.up(lwd = lwd, lty = lty),
        name="up.petal",
        single = T)
 value$tag = list("compose2")
 attr(value, "class") <- "cell"
 value
}
down.petal = function(lwd = 3, lty = 1) {
  value =  cof(v.arc.left.down(lwd = lwd, lty = lty),
               v.arc.right.down(lwd = lwd, lty = lty),
               name="down.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "cell"
  value
}

left.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.left.down(lwd = lwd, lty = lty),
               h.arc.left.up(lwd = lwd, lty = lty),
               name="left.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "cell"
  value
}
right.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.right.down(lwd = lwd, lty = lty),
               h.arc.right.up(lwd = lwd, lty = lty),
               name="right.petal",
               single = T)
  value$tag = list("compose2")
  attr(value, "class") <- "cell"
  value
}
flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down.petal(lwd = lwd, lty = lty),
              left.petal(lwd = lwd, lty = lty),
              right.petal(lwd = lwd, lty = lty))
  value$tag = list("compose4")
  attr(value, "class") = "cell"
  value
}
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


