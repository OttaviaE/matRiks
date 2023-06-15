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
 value =  cof(v.arc.left.up(lwd = lwd, lty = lty),
         v.arc.right.up(lwd = lwd, lty = lty),
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
  value =  cof(v.arc.left.down(lwd = lwd, lty = lty),
               v.arc.right.down(lwd = lwd, lty = lty),
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
  value =  cof(h.arc.left.down(lwd = lwd, lty = lty),
               h.arc.left.up(lwd = lwd, lty = lty),
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
  value =  cof(h.arc.right.down(lwd = lwd, lty = lty),
               h.arc.right.up(lwd = lwd, lty = lty),
               name="right_petal",
               single = T)
  value$tag = list("compose2", "d.int", "right", "petal")
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a flower
#'
#' Define the coordinates of the circle sections composing a flower
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a flower made of petals
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a right petal
#' flower()
#'
#' # change the line type of the right petal
#'
#' flower(lty = 3)
#' }
flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty))
  value$tag = list("compose4", "d.int")
  attr(value, "class") = "figure"
  value
}
#' Define the coordinates a single flower
#'
#' Define the coordinates of the circle sections composing a flower, which is forced to be a single figure (to be used in shape())
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single flower
#' @export
#'
#' @examples
#' \dontrun{
#' # return the default coordinates for drawing a single flower
#' s.flower()
#'
#' # change the line type of the single flower
#'
#' s.flower(lty = 3)
#' }
s_flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty),
              name = "flower",
              single = TRUE)
  value$tag = list("simple", "d.int")
  attr(value, "class") = "figure"
  value
}


