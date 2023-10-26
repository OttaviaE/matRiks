#' Coordinates of lines
#'
#' Define the coordinates for drawing lines
#'
#' vline() Define the coordinates for drawing a vertical line
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param pos.x numeric, position on the x axis. Default is 0.
#' @param pos.y numeric, position the y axis, Default is 0.
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param vis integer, define the visibility of the figure (default is 1, visible).
#'
#' @return Return the coordinates for drawing a vertical line
#' @export
#'
#' @examples
#' # default coordinates of a vertical line
#' vline()
#' # draw a vertical line with different lty
#' draw(vline(lty = 2))
vline <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = size.x,
                  pos.x=0 ,pos.y=0,
                  lty = 1, lwd =3,
                  vis = 1) {
  value <- list(
    shape = "vline",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi + pi / 2),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list(c("simple",'rotate'))
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vline description Coordinates of an horizontal line
#'
#' Define the coordinates for drawing an horizontal line
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, position on the x axis. Default is 0.
#' @param pos.y numeric, position the y axis, Default is 0.
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param vis integer, define the visibility of the figure (default is 1, visible).
#'
#' @return Return the coordinates for drawing an horizontal line
#' @export
#'
#' @examples
#' # default coordinates of an horizontal line
#' hline()
#' # draw a vertical line with different lty
#' draw(hline(lty = 2))
hline <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = size.x,
                  pos.x=0 ,pos.y=0,
                  lty = 1, lwd =3,
                  vis = 1) {
  value <- list(
    shape = "hline",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(pi),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list(c("simple", 'rotate') )
  )
  attr(value, "class") <- "figure"
  value
}
#' Coordinates of a cross
#'
#' Define the coordinates for drawing a cross
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a cross
#' @export
#'
#' @examples
#' # default coordinates of an horizontal line
#' cross()
#' # draw a vertical line with different lty
#' draw(cross(lty = 2))
cross <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = size.x,
                  lwd = 3, lty = 1) {
  value <-cof(vline(size.x = size.x,
                    size.y = size.y, lwd = lwd, lty = lty),
              hline(size.x = size.x,
                    size.y = size.y, lwd = lwd, lty = lty),
              single = TRUE,
              name = "cross")
  value$tag <- list(c("simple", 'rotate'))
  value$visible<-1
  attr(value, "class") <- "figure"
  value
}

#' @describeIn square Coordinates of a square composed of 4 lines
#'
#' Define the coordinates for drawing a square composed of 4 lines
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, position on the x axis. Default is 0.
#' @param pos.y numeric, position the y axis, Default is 0.
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a square composed of 4 lines
#' @export
#'
#' @examples
#' # default coordinates of square composed of 4 lines
#' square4()
#' # draw square composed of 4 lines with different lty
#' draw(square4(lty = 2))
square4 <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                    size.y = size.x,
                    pos.x = size.x,
                    pos.y = size.x,
                    lwd = 3, lty = 1) {
  value <-cof(vline(size.x = size.x,
                    size.y = size.y, pos.x=-pos.x,
                    lty = lty, lwd = lwd),
              vline(size.x = size.x,
                    size.y = size.y, pos.x=pos.x,
                    lty = lty, lwd = lwd),
              hline(size.x = size.x,
                    size.y = size.y, pos.y=-pos.y,
                    lty = lty, lwd = lwd),
              hline(size.x = size.x,
                    size.y = size.y, pos.y=pos.y,
              lty = lty, lwd = lwd))
  value$tag <- list(c("compose4", "d.ext"))
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vline Coordinates of the main diagonal line
#'
#' Define the coordinates for drawing the main diagonal line
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, position on the x axis. Default is 0.
#' @param pos.y numeric, position the y axis, Default is 0.
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param rotation integer, rotation of the line. Default is \eqn{\frac{(\pi - \pi)}{4}}
#' @param vis integer, define the visibility of the figure (default is 1, visible).
#'
#' @return Return the coordinates for drawing the main diagonal line
#' @export
#'
#' @examples
#' # default coordinates of the main diagonal line
#' diagline()
#' # draw the main diagonal line with different lty
#' draw(diagline(lty = 2))
diagline <- function(size.x=list(sqrt(square()$ size.x[[1]]^2 /2)),
                     size.y=size.x,
                     pos.x=0 ,pos.y=0,
                     lty = 1, lwd =3,
                     rotation = pi-pi/4,
                     vis = 1) {
  value <- list(
    shape = "diagline_inv",
    size.x = size.x,
    size.y = size.y,
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rotation),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list(c("simple", 'rotate') )
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn vline Coordinates of the inverse diagonal line
#'
#' Define the coordinates for drawing the inverse diagonal line
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, position on the x axis. Default is 0
#' @param pos.y numeric, position the y axis, Default is 0
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#' @param rotation integer, rotation of the line. Default is \eqn{\frac{(\pi + \pi)}{4}}
#' @param vis integer, define the visibility of the figure (default is 1, visible)
#' @return Return the coordinates for drawing the inverse diagonal line
#' @export
#'
#' @examples
#' # default coordinates of the main diagonal line
#' diagline_inv()
#' # draw the main diagonal line with different lty
#' draw(diagline_inv(lty = 2))
diagline_inv <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                         size.y = size.x,
                         pos.x=0 ,pos.y=0,
                         lty = 1, lwd =3,
                         rotation = pi + pi / 4,
                         vis = 1) {
  value <- list(
    shape = "diagline_inv",
    size.x = list(size.x),
    size.y = list(size.y),
    theta.1  = list(0),
    theta.2  = list(0),
    rotation = list(rotation),
    pos.x = list(pos.x),
    pos.y = list(pos.y),
    lty = list(lty),
    lwd = list(lwd),
    num = list(1),
    nv =  list(2),
    shade = list(NA),
    visible = vis,
    tag = list(c("simple", 'rotate') )
  )
  attr(value, "class") <- "figure"
  value
}

#' @describeIn cross Coordinates of an X
#'
#' Define the coordinates for drawing an X
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is sqrt(square()$ size.x[[1]]^2 /2)
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is size.x.
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line
#'
#' @return Return the coordinates for drawing an X
#' @export
#'
#' @examples
#' # default coordinates of an X
#' X()
#' # draw an X with different lty
#' draw(X(lty = 2))
X <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
              size.y =  size.x,
              lwd = 3, lty = 1) {
  value <-cof(diagline(size.x = size.x, size.y = size.y, lwd = lwd, lty = lty),
              diagline_inv(size.x = size.x, size.y = size.y, lwd = lwd, lty = lty),
              single = TRUE,name = "X")
  value$tag <- list(c("simple", 'rotate', "d.int"))
  value$visible<-1
  attr(value, "class") <- "figure"
  value
}
#' @describeIn luck Coordinates of a luck composed of 4 lines
#'
#' Define the coordinates for drawing of a luck composed of 4 lines
#'
#' @param size.x numeric, define the semi-major axis of the ellipse within which the figure is inscribed. Default is 10
#' @param size.y numeric, define the semi-minor axis of the ellipse within which the figure is inscribed. Default is 7
#' @param lwd integer, define the line width of the figure. Default is 3
#' @param lty integer, define the line type of the figure, default is 1 (solid line)
#'
#' @return Return the coordinates for drawing a luck composed of 4 lines
#' @export
#'
#' @examples
#' # default coordinates of an luck composed of 4 lines
#' luck4()
#' # draw a luck composed of 4 lines with different lty
#' draw(luck4(lty = 2))
luck4 <- function(size.x = 10,
                  size.y = 7,
                  lwd = 3, lty = 1) {
  value <-cof(diagline(size.x = size.x, pos.x = -size.y, pos.y = -size.y),
              diagline_inv(size.x = size.x, pos.x = -size.y, pos.y = size.y),
              diagline(size.x = size.x, pos.x = size.y, pos.y = size.y),
              diagline_inv(size.x = size.x, pos.x = size.y, pos.y = -size.y))
  value$tag <- list(c("compose4",'rotate', "d.ext"))
  attr(value, "class") <- "figure"
  value
}
