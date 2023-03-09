#' Eight shapes
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
vertical.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.vertical.inv(lwd = lwd, lty = lty),
                s.vertical(lwd = lwd, lty))
  value$tag <- list("compose4")
  attr(value, "class") <- "cell"
  value
}
horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.horizontal.inv(lwd = lwd, lty = lty),
                s.horizontal(lwd = lwd, lty))
  value$tag <- list("compose4")
  attr(value, "class") <- "cell"
  value
}

s.vertical.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.vertical.inv(lwd = lwd, lty = lty),
                s.s.vertical(lwd = lwd, lty),
                name = "s.vertical.eight",
                single = T)
  value$tag <- list("compose2")
  attr(value, "class") <- "cell"
  value
}
s.horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.horizontal.inv(lwd = lwd, lty = lty),
                s.s.horizontal(lwd = lwd, lty),
                name = "s.horizontal.eight",
                single = T)
  value$tag <- list("compose2")
  attr(value, "class") <- "cell"
  value
}
