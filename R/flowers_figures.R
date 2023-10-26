#' Define the coordinates of a lily
#'
#' Define the coordinates for drawing the circle arches composing a lily
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates drawing the circle arches composing a lily
#' @export
#'
#' @examples
#' # return the default coordinates drawing the circle arches composing a lily
#' lily()
#' # change the line type of the lily
#' lily(lty = 3)
lily <- function(lwd = 3, lty = 1) {
  value <-cof( horizontal_eight(lwd = lwd, lty = 1),
               vertical_eight(lwd = lwd, lty = 1))
  value$tag <- list(c("compose4", "d.int"))
  attr(value, "class") <- "figure"
  value
}


#' @describeIn lily Define the coordinates a single lily
#'
#' Define the coordinates for drawing the circle arches composing a single lily, to be used in shape()
#'
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing the circle arches composing a single lily, to be used in shape()
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single lily
#' s_lily()
#' # change the line type of the single lily
#' s_lily(lty = 3)
s_lily <- function(lwd = 3, lty = 1) {
  value <-cof( s_horizontal_eight(lwd = lwd, lty = 1),
               s_vertical_eight(lwd = lwd, lty = 1),
               name = "s_lily",
               single = TRUE)
  value$tag <- list(c("simple", "d.int"))
  attr(value, "class") <- "figure"
  value
}


#' Define the coordinates of petals
#'
#' Define the coordinates for drawing the circle arches composing some petals
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing an up petal
#' @export
#'
#' @examples
#' # return the default coordinates for drawing the circle arches composing an up petal
#' up_petal()
#' # change the line type of the up petal
#' up_petal(lty = 3)
up_petal <- function(lwd = 3, lty = 1) {
 value =  cof(v_arc_left_up(lwd = lwd, lty = lty),
        v_arc_right_up(lwd = lwd, lty = lty),
        name="up_petal",
        single = TRUE)
 value$tag = list(c("compose2", "d.int", "up", "petal"))
 attr(value, "class") <- "figure"
 value
}


#' @describeIn up_petal Define the coordinates of a down petal
#'
#' Define the coordinates for drawing the circle arches composing a down petal
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing  the circle arches composing a down petal
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a down petal
#' down_petal()
#' # change the line type of the down petal
#' down_petal(lty = 3)
down_petal <- function(lwd = 3, lty = 1) {
  value =  cof(v_arc_left_down(lwd = lwd, lty = lty),
              v_arc_right_down(lwd = lwd, lty = lty),
               name="down_petal",
               single = T)
  value$tag = list(c("compose2", "d.int", "down", "petal"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn up_petal Define the coordinates of a left petal
#'
#' Define the coordinates for drawing the circle arches composing a left petal
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing the circle arches composing a left petal
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a left petal
#' left_petal()
#' # change the line type of the left petal
#' left_petal(lty = 3)
left_petal <- function(lwd = 3, lty = 1) {
  value =  cof(h_arc_left_down(lwd = lwd, lty = lty),
               h_arc_left_up(lwd = lwd, lty = lty),
               name="left_petal",
               single = T)
  value$tag = list(c("compose2", "d.int", "left", "petal"))
  attr(value, "class") <- "figure"
  value
}
#' @describeIn up_petal Define the coordinates of a right petal
#'
#' Define the coordinates for drawing the circle arches composing a right petal
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing the circle arches composing a right petal
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a right petal
#' right_petal()
#' # change the line type of the right petal
#' right_petal(lty = 3)
right_petal <- function(lwd = 3, lty = 1) {
  value =  cof(h_arc_right_down(lwd = lwd, lty = lty),
               h_arc_right_up(lwd = lwd, lty = lty),
               name="right_petal",
               single = T)
  value$tag = list(c("compose2", "d.int", "right", "petal"))
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a miley
#'
#' Define the coordinates for drawing the petals composing a miley
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing the petals composing a miley
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a right petal
#' miley()
#' # change the line type of the right petal
#' miley(lty = 3)
miley <- function(lwd = 3, lty = 1) {
  value = cof(down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              up_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty))
  value$tag = list(c("compose4", "d.int"))
  attr(value, "class") = "figure"
  value
}
#' @describeIn miley Define the coordinates a single miley
#'
#' Define the coordinates for drawing the petals composing a single miley, to be used in shape()
#'
#' @inheritParams lily
#'
#' @return Return the coordinates for drawing the petals composing a single miley
#' @export
#'
#' @examples
#' # return the default coordinates for drawing the petals composing a single miley
#' s_miley()
#' # change the line type of the single miley
#' s_miley(lty = 3)
s_miley <- function(lwd = 3, lty = 1) {
  value = cof(down_petal(lwd = lwd, lty = lty),
              left_petal(lwd = lwd, lty = lty),
              up_petal(lwd = lwd, lty = lty),
              right_petal(lwd = lwd, lty = lty),
              name = "miley",
              single = TRUE)
  value$tag = list(c("simple", "d.int"))
  attr(value, "class") = "figure"
  value
}


