#'  Define the cooordinates for a vertical s
#'
#' @param lwd
#' @param lty
#'
#' @return
#' @export
#'
#' @examples Arrivano
s.vertical <- function(lty= 1,
                       lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd),
               v.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for a vertical inverted s
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.vertical.inv <- function(lty =1,
                           lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd),
               v.arc.left.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for an horizontal s
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.horizontal <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.up(lty = lty, lwd = lwd), h.arc.right.down(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for an horizontal invertred s
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.horizontal.inv <- function(lty = 1, lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty, lwd = lwd),
               h.arc.right.up(lty = lty, lwd = lwd))
  value$tag <- list("compose2","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for a single vertical s (to be used in diff_shapes)
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.s.vertical <- function(lty= 1,
                         lwd = 3) {
  value <-cof( v.arc.left.up(lty = lty, lwd = lwd), v.arc.right.down(lty = lty, lwd = lwd),
               single=TRUE, name="s.vertical")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for a single inverted vertical s (to be used in diff_shapes)
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.s.vertical.inv <- function(lty= 1,
                             lwd = 3) {
  value <-cof( v.arc.right.up(lty = lty, lwd = lwd), v.arc.left.down(lty = lty, lwd = lwd),single=TRUE,
               name="s.vertical.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for a single horizontal s (to be used in diff shapes)
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.s.horizontal <- function(lty= 1,
                           lwd = 3) {
  value <-cof(h.arc.left.up(lty = lty, lwd = lwd),
              h.arc.right.down(lty = lty, lwd = lwd),
              name="s.horizontal",
              single=TRUE)
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}
#'  Define the cooordinates for a single inverted horizontal s (to be used in diff shapes)
#'
#' @param lty
#' @param lwd
#'
#' @return
#' @export
#'
#' @examples
s.s.horizontal.inv <- function(lty= 1,
                               lwd = 3) {
  value <-cof( h.arc.left.down(lty = lty,  lwd = lwd),
               h.arc.right.up(lty = lty,  lwd = lwd),single=TRUE,
               name="s.s.horizontal.inv")
  value$tag <- list("simple","fill")
  attr(value, "class") <- "cell"
  value
}

