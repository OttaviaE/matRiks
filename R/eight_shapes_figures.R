#' Eight-shaped figures
#'
#' Define the coordinates for drawing eight-shaped figures
#' vertical_eight defines the coordinates for drawing a vertical eight-shaped figures.
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a vertical eight-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the vertical eight-shaped figure
#' vertical_eight()
#' # change the line type
#' vertical_eight(lty = 2)
vertical_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_vertical_s_inv(lwd = lwd, lty = lty),
                s_vertical_s(lwd = lwd, lty))
  value$tag <- list(c("compose2", "d.int", "vert", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vertical_eight Coordinates of an horizontal eight
#'
#' Define the coordinates for drawing an horizontal eight-shaped figure
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an horizontal eight-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the horizontal eight-shaped figure
#' horizontal_eight()
#' # change the line type
#' horizontal_eight(lty = 2)
horizontal_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_horizontal_s_inv(lwd = lwd, lty = lty),
                s_horizontal_s(lwd = lwd, lty))
  value$tag <- list(c("compose2", "d.int", "hor", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vertical_eight Coordinates of a single vertical eight
#'
#' Define the coordinates for drawing a single vertical eight-shaped figure, to be used in shape()
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single vertical eight-shaped figure to be used in shape()
#' @export
#'
#' @examples
#' # default coordinates of the single vertical eight-shaped figure
#' s_vertical_eight()
#' # change the line type
#' s_vertical_eight(lty = 2)
s_vertical_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_vertical_s_inv(lwd = lwd, lty = lty),
                s_vertical_s(lwd = lwd, lty),
                name = "s_vertical_eight",
                single = T)
  value$tag <- list(c("simple", "d.int", "vert", "eight"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vertical_eight Coordinates of a single horizontal eight
#'
#' Define the coordinates for drawing a single vertical eight-shaped figure, to be used in shape()
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single horizontal eight-shaped figure to be used in shape()
#' @export
#'
#' @examples
#' # default coordinates of a single horizontal eight-shaped figure
#' s_horizontal_eight()
#' # change the line type
#' s_horizontal_eight(lty = 2)
s_horizontal_eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s_horizontal_s_inv(lwd = lwd, lty = lty),
                s_horizontal_s(lwd = lwd, lty),
                name = "s_horizontal_eight",
                single = T)
  value$tag <- list(c("simple", "d.int", "hor", "eight"))
  attr(value, "class") <- "figure"
  value
}
