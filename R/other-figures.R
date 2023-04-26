#' Coordinates of a vertical bow tie
#'
#' Define the coordinates for drawing a vertical bow tie composed of two triangles
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a vertical bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a vertical bow tie
#' bow.tie()
#'
#' # change the coordinates for drawing a smaller bow tie
#'
#' bow.tie(size.x = 5)
#' }
bow.tie <- function(size.x = 10,
                    size.y = 10, pos.x = 0, shd = NA, lty = 1, lwd = 3) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+10, rot=pi/6,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              triangle(pos.x = pos.x, pos.y = pos.x-10, rot=pi/2,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "cell"
  value
}

#' Coordinates of a single vertical bow tie
#'
#' Define the coordinates for drawing a single vertical bow tie composed of two triangles, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return Return the coordinates for drawing a single vertical bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a bow tie
#' s.bow.tie()
#'
#' # change the coordinates for drawing a smaller bow tie
#'
#' bow.tie(size.x = 5)
#' }
s.bow.tie <- function(size.x = 10,
                      size.y = size.x, pos.x = 0,
                      shd = NA, lty = 1, lwd = 3) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+10, rot=pi/6,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              triangle(pos.x = pos.x, pos.y = pos.x-10, rot=pi/2,
                       size.x = size.x, size.y=size.y, shd = shd,
                       lty = lty, lwd = lwd),
              name = "s.bow.tie",
              single = T)
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "cell"
  value
}


#' Coordinates of an horizontal bow tie
#'
#' Define the coordinates for drawing an horizontal bow tie composed of two triangles
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a vertical bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a vertical bow tie
#' bow.tie.inv()
#'
#' # change the coordinates for drawing a smaller bow tie
#'
#' bow.tie.inv(size.x = 5)
#' }
bow.tie.inv <- function(size.x = 10, size.y = size.x, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
  value <-cof(triangle(pos.x = pos.x+size.x,
                       pos.y = pos.x,
                       rot=pi/3,
                       size.x = size.x, size.y=size.x,
                       shd = shd, lty = lty, lwd = lwd),
              triangle(pos.x = pos.x-size.x,
                       pos.y = pos.x,
                       rot=-pi,
                       size.x = size.x, size.y=size.x, shd = shd,
                       lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "figure"
  value
}



#' Coordinates of a single horizontal bow tie
#'
#' Define the coordinates for drawing a single horizontal bow tie composed of two triangles, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a single horizontal bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single horizontal bow tie
#' s.bow.tie.inv()
#'
#' # change the coordinates for drawing a smaller bow tie
#'
#' s.bow.tie.inv(size.x = 5)
#' }
s.bow.tie.inv <- function(size.x = 10, size.y = size.x, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
  value <-cof(triangle(pos.x = pos.x+size.x,
                       pos.y = pos.x,
                       rot=pi/3,
                       size.x = size.x, size.y=size.x,
                       shd = shd, lty = lty, lwd = lwd),
              triangle(pos.x = pos.x-size.x,
                       pos.y = pos.x,
                       rot=-pi,
                       size.x = size.x, size.y=size.x, shd = shd,
                       lty = lty, lwd = lwd),
              name = "s.bow.tie.inv",
              single = TRUE)
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "figure"
  value
}

#' Coordinates of a Malta cross
#'
#' Define the coordinates for drawing a Malta cross
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a single horizontal bow tie
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a Malta cross
#' malta()
#'
#' # change the coordinates for drawing a smaller Malta cross
#'
#' malta(size.x = 5)
#' }
malta = function(size.x = 10, size.y = size.y, pos.x = 0, shd = NA, lwd = 3, lty = 1){
  value = cof(bow.tie(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              bow.tie.inv(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty))
  value$tag = list("compose4")
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a Malta cross
#'
#' Define the coordinates for drawing a single Malta cross, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is size.x.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a single Malta cross
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single Malta cross
#' s.malta()
#'
#' # change the coordinates for drawing a smaller single Malta cross
#'
#' s.malta(size.x = 5)
#' }
s.malta = function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1){
  value = cof(s.bow.tie(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              s.bow.tie.inv(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              single = TRUE,
              name = "s.malta")
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}



#' Coordinates of an axe
#'
#' Define the coordinates for drawing an axe
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing an axe
#' @export
#'
#' @examples
#' # return the default coordinates for drawing an axe
#' axe()
#'
#' # change the coordinates for drawing a smaller single Malta cross
#'
#' axe(size.x = 5)
#' }
axe = function(size.x = 15, pos.x = 0, pos.y = 0, lty = 1, lwd = 3, shd = NA) {
  value = cof(slice(size.x = size.x,
                    pos.x = pos.x, pos.y = pos.y,
                    lwd = lwd, lty = lty, shd = shd),
              rotate(slice(size.x = size.x,
                             pos.x = pos.x, pos.y = pos.y,
                             lwd = lwd, lty = lty, shd = shd), 5))
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a single axe
#'
#' Define the coordinates for drawing a single axe, which is forced to be a single figure (to be used in shape())
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 10.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing an axe
#' @export
#'
#' @examples
#' # return the default coordinates for drawing an axe
#' axe()
#'
#' # change the coordinates for drawing a smaller single Malta cross
#'
#' axe(size.x = 5)
#' }
s.axe = function(size.x = 15, pos.x = 0, pos.y = 0, lty = 1, lwd = 3, shd = NA) {
  value = cof(slice(size.x = size.x,
                    pos.x = pos.x, pos.y = pos.y,
                    lwd = lwd, lty = lty, shd = shd),
              rotate(slice(size.x = size.x,
                             pos.x = pos.x, pos.y = pos.y,
                             lwd = lwd, lty = lty, shd = shd), 5),
              name = "s.axe",
              single = TRUE)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a maxi
#'
#' Define the coordinates for drawing a maxi (i.e., a cross composed of four lucks)
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 8
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 4.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a maxi
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a maxi
#' maxi()
#'
#' # change the coordinates for drawing a smaller maxi
#'
#' maxi(size.x = 5)
#' }
maxi = function(size.x = 8, size.y = 4, pos.x =0, shd = NA, lty = 1, lwd = 3) {
  value = cof(luck(pos.x = pos.x+size.x, pos.y = pos.x, rot=pi,
                   size.x = size.x, size.y=size.y,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x-size.x, pos.y = pos.x, rot=-pi,
                   size.x = size.x, size.y=size.y,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x, pos.y = pos.x+size.x, rot=-pi,
                   size.x = size.y, size.y=size.x,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x, pos.y = pos.x-size.x, rot=-pi,
                   size.x = size.y, size.y=size.x,
                   shd = shd, lty = lty, lwd = lwd))
  value$tag = list("compose4")
  attr(value, "class") <- "figure"
  value
}


#' Coordinates of a single maxi
#'
#' Define the coordinates for drawing a single maxi (i.e., a cross composed of four lucks), which is forced to be a single figure (to be used in shape())
#'
#' @param size.x numeric, define the semi-major axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 8
#' @param size.y  numeric, define the semi-minor axis of the ellipse for the polygon(s) within which the figure is inscribed. Default is 4.
#' @param pos.x numeric, define the position on the x axis. Default is 0.
#' @param shd character, define the color of the figure. Default is NA, which results in a transparent figure
#' @param lwd integer, define the line width of the figure. Default is 3.
#' @param lty integer, define the line type of the figure, default is 1 (solid line).
#'
#' @return  Return the coordinates for drawing a maxi
#' @export
#'
#' @examples
#' # return the default coordinates for drawing a single maxi
#' s.maxi()
#'
#' # change the coordinates for drawing a smaller single maxi
#'
#' s.maxi(size.x = 5)
#' }
s.maxi = function(size.x = 8, size.y = 4, pos.x =0, shd = NA, lty = 1, lwd = 3) {
  value = cof(luck(pos.x = pos.x+size.x, pos.y = pos.x, rot=pi,
                   size.x = size.x, size.y=size.y,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x-size.x, pos.y = pos.x, rot=-pi,
                   size.x = size.x, size.y=size.y,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x, pos.y = pos.x+size.x, rot=-pi,
                   size.x = size.y, size.y=size.x,
                   shd = shd, lty = lty, lwd = lwd),
              luck(pos.x = pos.x, pos.y = pos.x-size.x, rot=-pi,
                   size.x = size.y, size.y=size.x,
                   shd = shd, lty = lty, lwd = lwd),
              single = T,
              names = "s.maxi")
  value$tag = list("compose4")
  attr(value, "class") <- "figure"
  value
}

#' Cooordinates of a panthom figure
#'
#' Draw an empty figure
#'
#' @return
#' @export
#'
#' @examples
phantom <- function() {
  value <- list(
    shape = "phantom",
    size.x = list(5),
    size.y = list(5),
    theta.1= list(0),
    theta.2= list(0),
    rotation = list(pi),
    pos.x = list(0),
    pos.y = list(0),
    lty = list(0),
    lwd = list(1),
    num = list(1),
    nv = list(101),
    shade = list(NA),
    visible = 0,
    tag=list(c('simple'))
  )
  attr(value, "class") <- "field"
  value
}
