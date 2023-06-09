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
#' vertical.eight()
#' # change the line type
#' vertical.eight(lty = 2)
#' }
vertical.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  vertical.s.inv(lwd = lwd, lty = lty),
                vertical.s(lwd = lwd, lty))
  value$tag <- list("compose4", "d.int")
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
#' horizontal.eight()
#' # change the line type
#' horizontal.eight(lty = 2)
#' }
horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  horizontal.s.inv(lwd = lwd, lty = lty),
                horizontal.s(lwd = lwd, lty))
  value$tag <- list("compose4", "d.int")
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
#' vertical.s.eight()
#' # change the line type
#' vertical.s.eight(lty = 2)
#' }
vertical.s.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.vertical.s.inv(lwd = lwd, lty = lty),
                s.vertical.s(lwd = lwd, lty),
                name = "vertical.s.eight",
                single = T)
  value$tag <- list("compose2", "d.int")
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
#' horizontal.s.eight()
#' # change the line type
#' horizontal.s.eight(lty = 2)
#' }
horizontal.s.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.horizontal.s.inv(lwd = lwd, lty = lty),
                s.horizontal.s(lwd = lwd, lty),
                name = "horizontal.s.eight",
                single = T)
  value$tag <- list("compose2", "d.int")
  attr(value, "class") <- "figure"
  value
}
