#' Coordinates of a vertical eight
#'
#' Define the coordinates of a vertical eight
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return
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
  value <-cof(  s.vertical.inv(lwd = lwd, lty = lty),
                s.vertical(lwd = lwd, lty))
  value$tag <- list("compose4")
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
#' @return
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
  value <-cof(  s.horizontal.inv(lwd = lwd, lty = lty),
                s.horizontal(lwd = lwd, lty))
  value$tag <- list("compose4")
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
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of te single vertical eight
#' s.vertical.eight()
#' # change the line type
#' s.vertical.eight(lty = 2)
#' }
s.vertical.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.vertical.inv(lwd = lwd, lty = lty),
                s.s.vertical(lwd = lwd, lty),
                name = "s.vertical.eight",
                single = T)
  value$tag <- list("compose2")
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
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of a single horizontal eight
#' s.horizontal.eight()
#' # change the line type
#' s.horizontal.eight(lty = 2)
#' }
s.horizontal.eight <- function(lwd = 3, lty = 1) {
  value <-cof(  s.s.horizontal.inv(lwd = lwd, lty = lty),
                s.s.horizontal(lwd = lwd, lty),
                name = "s.horizontal.eight",
                single = T)
  value$tag <- list("compose2")
  attr(value, "class") <- "figure"
  value
}
