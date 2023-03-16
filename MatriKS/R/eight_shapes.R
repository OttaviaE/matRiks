#' Define the coordinates of a vertical eight
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
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of an horizontal eight
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.horizontal.inv(lwd = lwd, lty = lty),
                s.horizontal(lwd = lwd, lty))
  value$tag <- list("compose4")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a single vertical eight (to be used in diff_shapes)
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.vertical.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.vertical.inv(lwd = lwd, lty = lty),
                s.s.vertical(lwd = lwd, lty),
                name = "s.vertical.eight",
                single = T)
  value$tag <- list("compose2")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a single horizontal eight (to be used in diff_shapes)
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.horizontal.inv(lwd = lwd, lty = lty),
                s.s.horizontal(lwd = lwd, lty),
                name = "s.horizontal.eight",
                single = T)
  value$tag <- list("compose2")
  attr(value, "class") <- "figure"
  value
}
