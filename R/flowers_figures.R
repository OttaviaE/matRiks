#' Define the coordinates of a lily
#'
#' Define the coordinates of the circle sections composing a lily
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a lily
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a lily
#' lily()
#'
#' # change the line type of the lily
#'
#' lily(lty = 3)
#' }
lily <- function(lwd = 3, lty = 1) {
  value <-cof( s_horizontal_eight(lwd = lwd, lty = 1),
               s_vertical_eight(lwd = lwd, lty = 1))
  value$tag <- list("compose2", "d.int")
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates a single lily
#'
#' Define the coordinates of the circle sections composing a lily, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single lily
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a single lily
#' s_lily()
#'
#' # change the line type of the single lily
#'
#' s_lily(lty = 3)
#' }
s_lily <- function(lwd = 3, lty = 1) {
  value <-cof( s_horizontal_eight(lwd = lwd, lty = 1),
               s_vertical_eight(lwd = lwd, lty = 1),
               name = "s_lily",
               single = TRUE)
  value$tag <- list("simple", "d.int")
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of an up petal
#'
#' Define the coordinates of the circle sections composing an up petal
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing an up petal
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing an up petal
#' up_petal()
#'
#' # change the line type of the up petal
#'
#' up_petal(lty = 3)
#' }
up_petal = function(lwd = 3, lty = 1) {
 value =  cof(v_arc_left_up(lwd = lwd, lty = lty),
        v_arc_right_up(lwd = lwd, lty = lty),
        name="up_petal",
        single = TRUE)
 value$tag = list("compose2", "d.int", "up", "petal")
 attr(value, "class") <- "figure"
 value
}


#' Define the coordinates of a down petal
#'
#' Define the coordinates of the circle sections composing a down petal
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a down petal
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a down petal
#' down_petal()
#'
#' # change the line type of the down petal
#'
#' down_petal(lty = 3)
#' }
down_petal = function(lwd = 3, lty = 1) {
  value =  cof(v_arc_left_down(lwd = lwd, lty = lty),
              v_arc_right_down(lwd = lwd, lty = lty),
               name="down_petal",
               single = T)
  value$tag = list("compose2", "d.int", "down", "petal")
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a left petal
#'
#' Define the coordinates of the circle sections composing a left petal
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a left petal
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a left petal
#' left_petal()
#'
#' # change the line type of the left petal
#'
#' left_petal(lty = 3)
#' }
left_petal = function(lwd = 3, lty = 1) {
  value =  cof(h_arc_left_down(lwd = lwd, lty = lty),
               h_arc_left_up(lwd = lwd, lty = lty),
               name="left_petal",
               single = T)
  value$tag = list("compose2", "d.int", "left", "petal")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a right petal
#'
#' Define the coordinates of the circle sections composing a right petal
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a right petal
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a right petal
#' right_petal()
#'
#' # change the line type of the right petal
#'
#' right_petal(lty = 3)
#' }
right_petal = function(lwd = 3, lty = 1) {
  value =  cof(h_arc_right_down(lwd = lwd, lty = lty),
               h_arc_right_up(lwd = lwd, lty = lty),
               name="right_petal",
               single = T)
  value$tag = list("compose2", "d.int", "right", "petal")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a miley
#'
#' Define the coordinates of the circle sections composing a miley
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a miley made of petals
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a right petal
#' miley()
#'
#' # change the line type of the right petal
#'
#' miley(lty = 3)
#' }
miley = function(lwd = 3, lty = 1) {
  value = cof(up_petal(lwd = lwd, lty = lty),
              down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty))
  value$tag = list(c("compose4", "d.int"))
  attr(value, "class") = "figure"
  value
}
#' Define the coordinates a single miley
#'
#' Define the coordinates of the circle sections composing a miley, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single miley
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a single miley
#' s.miley()
#'
#' # change the line type of the single miley
#'
#' s.miley(lty = 3)
#' }
s_miley = function(lwd = 3, lty = 1) {
  value = cof(up_petal(lwd = lwd, lty = lty),
              down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty),
              name = "miley",
              single = TRUE)
  value$tag = list(c("simple", "d.int"))
  attr(value, "class") = "figure"
  value
}


