#' Coordinates of a vertical eight
#'
#' Define the coordinates of a vertical eight
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a vertical eight
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the vertical eight
#' vertical_eight()
#' # change the line type
#' vertical_eight(lty = 2)
#' }
vertical_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_vertical_s_inv(lwd = lwd, lty = lty),
                s_vertical_s(lwd = lwd, lty))
  value$tag <- list(c("compose2", "d.int", "vert", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of an horizontal eight
#'
#' Define the coordinates of an horizontal eight
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an horizontal eight
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the horizontal eight
#' horizontal_eight()
#' # change the line type
#' horizontal_eight(lty = 2)
#' }
horizontal_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_horizontal_s_inv(lwd = lwd, lty = lty),
                s_horizontal_s(lwd = lwd, lty))
  value$tag <- list(c("compose2", "d.int", "hor", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a single vertical eight
#'
#' Define the coordinates of a single vertical eight, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single vertical eight to be used in shape()
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of te single vertical eight
#' s_vertical_eight()
#' # change the line type
#' s_vertical_eight(lty = 2)
#' }
s_vertical_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_vertical_s_inv(lwd = lwd, lty = lty),
                s_vertical_s(lwd = lwd, lty),
                name = "s_vertical_eight",
                single = T)
  value$tag <- list(c("compose2", "d.int", "vert", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a single horizontal eight
#'
#' Define the coordinates of a single vertical eight, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single horizontal eight to be used in shape()
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of a single horizontal eight
#' s_horizontal_eight()
#' # change the line type
#' s_horizontal_eight(lty = 2)
#' }
s_horizontal_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_horizontal_s_inv(lwd = lwd, lty = lty),
                s_horizontal_s(lwd = lwd, lty),
                name = "s_horizontal_eight",
                single = T)
  value$tag <- list(c("compose2", "d.int", "hor", "eight"))
  attr(value, "class") <- "figure"
  value
}
