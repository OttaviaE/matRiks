#' Coordinates of a vertical bow tie
#'
#' @param size.x An integer giving the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y An integer or a vector giving the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x Position on the x axis. Default is 0.
#' @param shd Color of the figure Default is NA which results in a transparent figure
#' @param lty An integer defining the border line. Default is 1 (solid), can be dotted (2) or dashed (3)
#' @param lwd An integer defining the width of the border line. Default is 3.
#'
#' @return  Return the coordinates for drawing a vertical bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a bow tie
#' bow.tie()
#'
#' # change the coordinates for drawing a smaller bow tie
#'
#' bow.tie(size.x = 5)
#' }
bow.tie <- function(size.x = 10,
                    size.y = 10, pos.x = 0, shd = NA, lty = 1, lwd = 3) {
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
