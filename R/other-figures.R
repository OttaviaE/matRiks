#' Define the cooordinates for a vertical bow tie
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
bow.tie <- function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+size.x,
                       rot=pi/6,
                       size.x = size.x, size.y=size.x,
                       shd = shd, lty = lty, lwd = lwd),
              triangle(pos.x = pos.x,
                       pos.y = pos.x-size.x,
                       rot=pi/2,
                       size.x = size.x, size.y=size.x, shd = shd,
                       lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "figure"
  value
}


#'  Define the cooordinates for an horizontal bow tie
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
bow.tie.inv <- function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
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


#'  Define the cooordinates for a single bow tie (to be used in diff_shapes)
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.bow.tie <- function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
  value <-cof(triangle(pos.x = pos.x, pos.y = pos.x+size.x,
                       rot=pi/6,
                       size.x = size.x, size.y=size.x,
                       shd = shd, lty = lty, lwd = lwd),
              triangle(pos.x = pos.x,
                       pos.y = pos.x-size.x,
                       rot=pi/2,
                       size.x = size.x, size.y=size.x, shd = shd,
                       lty = lty, lwd = lwd),
              name = "s.bow.tie",
              single = TRUE)
  value$tag <- list("compose2","fill", "rotate")
  attr(value, "class") <- "figure"
  value
}


#'  Define the cooordinates for a single bow tie inverse (to be used in diff_shapes)
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.bow.tie.inv <- function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1) {
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

#'  Define the cooordinates for a Malta cross
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
malta = function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1){
  value = cof(bow.tie(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              bow.tie.inv(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty))
  value$tag = list("compose4")
  attr(value, "class") <- "figure"
  value
}


#'  Define the cooordinates for a single Malta cross (to be used in diff_shapes)
#'
#' @param size.x
#' @param pos.x
#' @param shd
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
s.malta = function(size.x = 10, pos.x = 0, shd = NA, lwd = 3, lty = 1){
  value = cof(s.bow.tie(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              s.bow.tie.inv(size.x = size.x, pos.x = pos.x, shd = shd, lwd = lwd, lty),
              single = TRUE,
              name = "s.malta")
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}



#'  Define the cooordinates for an axe
#'
#' @param size.x
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param shd
#'
#' @return
#' @export
#'
#' @examples
axe = function(size.x = 15, pos.x = 0, pos.y = 0, lty = 1, lwd = 3, shd = NA) {
  value = cof(slice(size.x = size.x,
                    pos.x = pos.x, pos.y = pos.y,
                    lwd = lwd, lty = lty, shd = shd),
              rotation(slice(size.x = size.x,
                             pos.x = pos.x, pos.y = pos.y,
                             lwd = lwd, lty = lty, shd = shd), 5))
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}


#'  Define the cooordinates for a single axe (to be used in diff_shapes)
#'
#' @param size.x
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param shd
#'
#' @return
#' @export
#'
#' @examples
s.axe = function(size.x = 15, pos.x = 0, pos.y = 0, lty = 1, lwd = 3, shd = NA) {
  value = cof(slice(size.x = size.x,
                    pos.x = pos.x, pos.y = pos.y,
                    lwd = lwd, lty = lty, shd = shd),
              rotation(slice(size.x = size.x,
                             pos.x = pos.x, pos.y = pos.y,
                             lwd = lwd, lty = lty, shd = shd), 5),
              name = "s.axe",
              single = TRUE)
  value$tag = list("compose2")
  attr(value, "class") <- "figure"
  value
}


#'  Define the cooordinates for a maxi
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
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


#'  Define the cooordinates for a single maxi (to be used in diff_shapes)
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param shd
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
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

#' Define the cooordinates of a panthom figure
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
