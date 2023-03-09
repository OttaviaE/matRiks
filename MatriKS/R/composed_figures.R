#' Define the coordinates of a biscuit
#'
#' @param pos.x
#' @param pos.y
#' @param size.x
#' @param size.y
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return Based on the chosen shape, return an object with the information for plotting the desired design. If the name is precedeed by an s, the object is seen as a unique object, otherwise it is seen as a combination of multiple objects
#' @export
#'
#' @examples One day
biscuit <- function(pos.x = 0, pos.y = 0, size.x = 10,
                    size.y = 10, shd = "black", lty = 1, lwd = 3) {
  value <-cof(hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, rot = 3*pi/2,
                      lty = lty,
                      lwd = lwd),
              hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, lty = lty, lwd = lwd))
  value$tag <- list("small", "compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a biscuit (to be used in diff_shapes)
#'
#' @param pos.x
#' @param pos.y
#' @param size.x
#' @param size.y
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.biscuit <- function(pos.x = 0, pos.y = 0, size.x = 10,
                    size.y = 10, shd = "black", lty = 1, lwd = 3) {
  value <-cof(hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, rot = 3*pi/2,
                      lty = lty,
                      lwd = lwd),
              hexagon(pos.x = pos.x,
                      pos.y = pos.y, size.x = size.x, size.y = size.y,
                      shd = shd, lty = lty, lwd = lwd),
              name = "s.biscuit",
              single = TRUE)
  value$tag <- list("small", "compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a vertical bow tie
#'
#' @param pos.x
#' @param size.x
#' @param size.y
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
bow.tie <- function(pos.x = 0, size.x = 10,
                    size.y = 10, shd = NA, lty = 1, lwd = 3) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+10, rot=pi/6,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              triangle(pos.x = pos.x, pos.y = pos.x-10, rot=pi/2,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "cell"
  value
}
#' Define the coordinates of a single vertical bow tie (to be used in diff_shapes)
#'
#' @param pos.x
#' @param size.x
#' @param size.y
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.bow.tie <- function(pos.x = 0, size.x = 10,
                    size.y = 10, shd = NA, lty = 1, lwd = 3) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+10, rot=pi/6,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              triangle(pos.x = pos.x, pos.y = pos.x-10, rot=pi/2,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              name = "s.bow.tie",
              single = T)
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "cell"
  value
}
