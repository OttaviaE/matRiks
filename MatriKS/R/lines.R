#' Define the coordinates of a vertical line
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param vis
#'
#' @return
#' @export
#'
#' @examples
vline <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = sqrt(square()$ size.y[[1]]^2 /2),
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
    tag = list("simple","fill",'rotate')
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of an horizontal line
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param vis
#'
#' @return
#' @export
#'
#' @examples
hline <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = sqrt(square()$ size.y[[1]]^2 /2),
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
    tag = list("simple","fill",'rotate' )
  )
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a cross
#'
#' @param size.x
#' @param size.y
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
cross <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                  size.y = sqrt(square()$ size.y[[1]]^2 /2),
                  lwd = 3, lty = 1) {
  value <-cof(vline(size.x = size.x,
                    size.y = size.y, lwd = lwd, lty = lty),
              hline(size.x = size.x,
                    size.y = size.y, lwd = lwd, lty = lty),
              single = TRUE,
              name = "cross")
  value$tag <- list("simple","fill",'rotate')
  value$visible<-1
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of a square composed of 4 lines
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
square4 <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                    size.y = sqrt(square()$ size.y[[1]]^2 /2),
                    pos.x = sqrt(square()$ size.x[[1]]^2 /2),
                    pos.y = sqrt(square()$ size.y[[1]]^2 /2),
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
  value$tag <- list("compose4",'rotate')
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of the main diagonal line
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param rotation
#' @param vis
#'
#' @return
#' @export
#'
#' @examples
diagline <- function(size.x=list(sqrt(square()$ size.x[[1]]^2 /2)),
                     size.y=list(sqrt(square()$ size.x[[1]]^2 /2)),
                     pos.x=0 ,pos.y=0,
                     lty = 1, lwd =3, rotation = pi-pi/4,
                     vis = 1) {
  value <- list(
    shape = "diagline.inv",
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
    tag = list("simple","fill",'rotate' )
  )
  attr(value, "class") <- "figure"
  value
}

#' Define the coordinates of the secondary diagonal line
#'
#' @param size.x
#' @param size.y
#' @param pos.x
#' @param pos.y
#' @param lty
#' @param lwd
#' @param rotation
#' @param vis
#'
#' @return
#' @export
#'
#' @examples
diagline.inv <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
                         size.y = sqrt(square()$ size.y[[1]]^2 /2),
                         pos.x=0 ,pos.y=0,
                         lty = 1, lwd =3,
                         rotation = pi + pi / 4,
                         vis = 1) {
  value <- list(
    shape = "diagline.inv",
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
    tag = list("simple","fill",'rotate' )
  )
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of an X
#'
#' @param size.x
#' @param size.y
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
X <- function(size.x = sqrt(square()$ size.x[[1]]^2 /2),
              size.y =  sqrt(square()$ size.y[[1]]^2 /2),
              lwd = 3, lty = 1) {
  value <-cof(diagline(size.x = size.x, size.y = size.y, lwd = lwd, lty = lty),
              diagline.inv(size.x = size.x, size.y = size.y, lwd = lwd, lty = lty),
              single = TRUE,name = "X")
  value$tag <- list("simple","fill",'rotate')
  value$visible<-1
  attr(value, "class") <- "figure"
  value
}
#' Define the coordinates of a luck composed of 4 lines
#'
#' @param size.x
#' @param size.y
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples
luck.4 = function(size.x = 10,
                  size.y = 7,
                  lwd = 3, lty = 1) {
  value <-cof(diagline(size.x = size.x, pos.x = size.y, pos.y = size.y),
               diagline.inv(size.x = size.x, pos.x = -size.y, pos.y = size.y),
               diagline.inv(size.x = size.x, pos.x = size.y, pos.y = -size.y),
               diagline(size.x = size.x, pos.x = -size.y, pos.y = -size.y))
  value$tag <- list("compose4",'rotate')
  attr(value, "class") <- "figure"
  value
}
