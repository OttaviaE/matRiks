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
  value <-cof( s.horizontal.eight(lwd = lwd, lty = 1),
               s.vertical.eight(lwd = lwd, lty = 1))
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
#' s.lily()
#'
#' # change the line type of the single lily
#'
#' s.lily(lty = 3)
#' }
s.lily <- function(lwd = 3, lty = 1) {
  value <-cof( s.horizontal.eight(lwd = lwd, lty = 1),
               s.vertical.eight(lwd = lwd, lty = 1),
               name = "s.lily",
               single = T)
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
#' up.petal()
#'
#' # change the line type of the up petal
#'
#' up.petal(lty = 3)
#' }
up.petal = function(lwd = 3, lty = 1) {
 value =  cof(v.arc.left.up(lwd = lwd, lty = lty),
         v.arc.right.up(lwd = lwd, lty = lty),
        name="up.petal",
        single = T)
 value$tag = list("compose2", "d.int")
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
#' down.petal()
#'
#' # change the line type of the down petal
#'
#' down.petal(lty = 3)
#' }
down.petal = function(lwd = 3, lty = 1) {
  value =  cof(v.arc.left.down(lwd = lwd, lty = lty),
               v.arc.right.down(lwd = lwd, lty = lty),
               name="down.petal",
               single = T)
  value$tag = list("compose2", "d.int")
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
#' left.petal()
#'
#' # change the line type of the left petal
#'
#' left.petal(lty = 3)
#' }
left.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.left.down(lwd = lwd, lty = lty),
               h.arc.left.up(lwd = lwd, lty = lty),
               name="left.petal",
               single = T)
  value$tag = list("compose2", "d.int")
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
#' right.petal()
#'
#' # change the line type of the right petal
#'
#' right.petal(lty = 3)
#' }
right.petal = function(lwd = 3, lty = 1) {
  value =  cof(h.arc.right.down(lwd = lwd, lty = lty),
               h.arc.right.up(lwd = lwd, lty = lty),
               name="right.petal",
               single = T)
  value$tag = list("compose2", "d.int")
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
              down.petal(lwd = lwd, lty = lty),
              left.petal(lwd = lwd, lty = lty),
              right.petal(lwd = lwd, lty = lty))
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
s.flower = function(lwd = 3, lty = 1) {
  value = cof(up.petal(lwd = lwd, lty = lty),
              down.petal(lwd = lwd, lty = lty),
              left.petal(lwd = lwd, lty = lty),
              right.petal(lwd = lwd, lty = lty),
              name = "flower",
              single = TRUE)
  value$tag = list("simple", "d.int")
  attr(value, "class") = "figure"
  value
}


