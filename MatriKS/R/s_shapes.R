#' Combination of arches (S shapes)
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples Arrivano
s.vertical <- function(lty= 1,
                       lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd),
               v.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
s.vertical.inv <- function(lty =1,
                           lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd),
               v.arc.left.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
s.horizontal <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.up(lty = lty, lwd = lwd), h.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
s.horizontal.inv <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty, lwd = lwd),
               h.arc.right.up(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
s.s.vertical <- function(lty= 1,
                         lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd), v.arc.right.down(lty = lty, lwd = lwd),
               single=TRUE, name="s.vertical")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
s.s.vertical.inv <- function(lty= 1,
                             lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd), v.arc.left.down(lty = lty, lwd = lwd),single=TRUE,
               name="s.vertical.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
s.s.horizontal <- function(lty= 1,
                           lwd = 3) {
  value <-cof(h.arc.left.up(lty = lty, lwd = lwd),
              h.arc.right.down(lty = lty, lwd = lwd),
              name="s.horizontal",
              single=TRUE)
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
s.s.horizontal.inv <- function(lty= 1,
                               lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty, lty = lty, lwd = lwd), h.arc.right.up(lty = lty, lty = lty, lwd = lwd),single=TRUE,
               name="s.horizontal.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
