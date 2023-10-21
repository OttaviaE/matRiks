#' Coordinates of S-shaped figures
#'
#' Define the coordinates for drawing S-shaped figures
#'
#' Define the coordinates of a vertical S-shaped figure
#'
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing a vertical S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the vertical S-shaped figure
#' vertical_s()
#' # change the line type
#' vertical_s(lty = 2)
vertical_s <- function(lty= 1,
                       lwd = 3) {
  value <-cof( v_arc_left_up(lty = lty, lwd = lwd),
               v_arc_right_down(lty = lty, lwd = lwd))
  value$tag <- list(c("compose2","d.int", "vert", "s_shape"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of an inverted vertical S-shaped figure
#'
#' Define the coordinates of an inverted vertical S-shaped figure
#'
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing an inverted vertical S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the inverted vertical S-shaped figure
#' vertical_s_inv()
#' # change the line type
#' vertical_s_inv(lty = 2)
vertical_s_inv <- function(lty =1,
                           lwd = 3) {
  value <-cof( v_arc_right_up(lty = lty, lwd = lwd),
               v_arc_left_down(lty = lty, lwd = lwd))
  value$tag <- list(c("compose2","d.int", "vert", "s_shape", "inv"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of an horizontal S-shaped figure
#'
#' Define the coordinates of an horizontal S-shaped figure
#'
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing an horizontal S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the horizontal S
#' horizontal_s()
#' # change the line type
#' horizontal_s(lty = 2)
horizontal_s <- function(lty = 1, lwd = 3) {
  value <-cof( h_arc_left_up(lty = lty, lwd = lwd),
               h_arc_right_down(lty = lty, lwd = lwd))
  value$tag <- list(c("compose2","d.int", "hor", "s_shape"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of an inverted horizontal S-shaped figure
#'
#' Define the coordinates of an inverted horizontal S-shaped figure
#'
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing an horizontal S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the horizontal S-shaped figure
#' horizontal_s_inv()
#' # change the line type
#' horizontal_s_inv(lty = 2)
horizontal_s_inv <- function(lty = 1, lwd = 3) {
  value <-cof( h_arc_left_down(lty = lty, lwd = lwd),
               h_arc_right_up(lty = lty, lwd = lwd))
  value$tag <- list(c("compose2","d.int", "hor", "s_shape", "inv"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of a single vertical S-shaped figure
#'
#' Define the coordinates for drawing a single vertical S-shaped figure composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a vertical S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the vertical S-shaped figure
#' s_vertical_s()
#' # change the line type
#' s_vertical_s(lty = 2)
s_vertical_s <- function(lty= 1,
                         lwd = 3) {
  value <-cof( v_arc_left_up(lty = lty, lwd = lwd),
               v_arc_right_down(lty = lty, lwd = lwd),
               single=TRUE, name="vertical_s")
  value$tag <- list(c("simple","d.int", "vert", "s_shape"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of a single inverted vertical S-shaped figure
#'
#' Define the coordinates for drawing a single inverted vertical S-shaped figure composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing a single vertical S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the single inverted vertical S-shaped figure
#' s_vertical_s_inv()
#' # change the line type
#' s_vertical_s_inv(lty = 2)
s_vertical_s_inv <- function(lty= 1,
                             lwd = 3) {
  value <-cof( v_arc_right_up(lty = lty, lwd = lwd),
               v_arc_left_down(lty = lty, lwd = lwd),single=TRUE,
               name="vertical_s_inv")
  value$tag <- list(c("simple","d.int", "vert", "s_shape", "inv"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of a single horizontal S-shaped figure
#'
#' Define the coordinates for drawing a single horizontal S-shaped figure  composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single horizontal S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the single horizontal S-shaped figure
#' s_horizontal_s()
#' # change the line type
#' s_horizontal_s(lty = 2)
s_horizontal_s <- function(lty= 1,
                           lwd = 3) {
  value <-cof(h_arc_left_up(lty = lty, lwd = lwd),
              h_arc_right_down(lty = lty, lwd = lwd),
              name="horizontal_s",
              single=TRUE)
  value$tag <- list("simple","d.int", "hor", "s_shape")
  attr(value, "class") <- "figure"
  value
}
#' @describeIn vertical_s Coordinates of a single inverted horizontal S-shaped figure
#'
#' Define the coordinates for drawing a single inverted horizontal S-shaped figure  composed of two arches, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single inverted horizontal S-shaped figure
#' @export
#'
#' @examples
#' # default coordinates of the single inverted horizontal S-shaped figure
#' s_horizontal_s_inv()
#' # change the line type
#' s_horizontal_s_inv(lty = 2)
s_horizontal_s_inv <- function(lty= 1,
                               lwd = 3) {
  value <-cof( h_arc_left_down(lty = lty,  lwd = lwd),
               h_arc_right_up(lty = lty,  lwd = lwd),single=TRUE,
               name="s_horizontal_s_inv")
  value$tag <- list(c("simple","d.int", "hor", "s_shape", "inv"))
  attr(value, "class") <- "figure"
  value
}

