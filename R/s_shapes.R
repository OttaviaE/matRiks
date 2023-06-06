#' Coordinates of a vertical S
#'
#' Define the coordinates of a vertical S
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a vertical S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the vertical S
#' vertical.s()
#' # change the line type
#' vertical.s(lty = 2)
#' }
vertical.s <- function(lty= 1,
                       lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd),
               v.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an inverted vertical S
#'
#' Define the coordinates of an inverted vertical S
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an inverted vertical S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the inverted vertical S
#' vertical.s.inv()
#' # change the line type
#' vertical.s.inv(lty = 2)
#' }
vertical.s.inv <- function(lty =1,
                           lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd),
               v.arc.left.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an horizontal S
#'
#' Define the coordinates of an horizontal S
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an horizontal S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the horizontal S
#' horizontal.s()
#' # change the line type
#' horizontal.s(lty = 2)
#' }
horizontal.s <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.up(lty = lty, lwd = lwd), h.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of an inverted horizontal S
#'
#' Define the coordinates of an inverted horizontal S
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an horizontal S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the horizontal S
#' horizontal.s.inv()
#' # change the line type
#' horizontal.s.inv(lty = 2)
#' }
horizontal.s.inv <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty, lwd = lwd),
               h.arc.right.up(lty = lty, lwd = lwd))
  value$tag <- list("compose2","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single vertical S
#'
#' Define the coordinates for drawing a single vertical S composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a vertical S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the vertical S
#' s.vertical.s()
#' # change the line type
#' s.vertical.s(lty = 2)
#' }
s.vertical.s <- function(lty= 1,
                         lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd), v.arc.right.down(lty = lty, lwd = lwd),
               single=TRUE, name="vertical.s")
  value$tag <- list("simple","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single inverted vertical S
#'
#' Define the coordinates for drawing a single inverted vertical S composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single vertical S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single inverted vertical S
#' s.vertical.s.inv()
#' # change the line type
#' s.vertical.s.inv(lty = 2)
#' }
s.vertical.s.inv <- function(lty= 1,
                             lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd), v.arc.left.down(lty = lty, lwd = lwd),single=TRUE,
               name="vertical.s.inv")
  value$tag <- list("simple","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single horizontal S
#'
#' Define the coordinates for drawing a single horizontal S composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single horizontal S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single horizontal S
#' s.horizontal.s()
#' # change the line type
#' s.horizontal.s(lty = 2)
#' }
s.horizontal.s <- function(lty= 1,
                           lwd = 3) {
  value <-cof(h.arc.left.up(lty = lty, lwd = lwd),
              h.arc.right.down(lty = lty, lwd = lwd),
              name="horizontal.s",
              single=TRUE)
  value$tag <- list("simple","d.int")
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a single inverted horizontal S
#'
#' Define the coordinates for drawing a single inverted horizontal S composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single inverted horizontal S
#' @export
#'
#' @examples
#' \dontrun{
#' # default coordinates of the single inverted horizontal S
#' s.horizontal.s.inv()
#' # change the line type
#' s.horizontal.s.inv(lty = 2)
#' }
s.horizontal.s.inv <- function(lty= 1,
                               lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty,  lwd = lwd),
               h.arc.right.up(lty = lty,  lwd = lwd),single=TRUE,
               name="s.horizontal.s.inv")
  value$tag <- list("simple","d.int")
  attr(value, "class") <- "figure"
  value
}

