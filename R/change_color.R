#' Change shade
#'
#' Change the shade of a figure
#'
#' @param obj The figure
#' @param ... other arguments
#'
#' @return Return the original figure with the inverted shade
#' @export change_color
#' @export
#'
#' @examples
#' # draw a square with inverted color
#' draw(change_color(square()))
change_color <- function(obj, ...) {
  UseMethod("change_color")
}


#' @describeIn change_color Change shade
#'
#' Change the shade of a figure
#'
#' @param obj The figure
#' @param ... other arguments
#'
#' @return Return the original figure with the inverted shade
#' @export change_color.figure
#' @export
#'
#' @examples
#' draw(change_color(square()))
change_color.figure <- function(obj, ...) {
  if (any(is.na(unlist(obj$shade))) == TRUE) {
    obj <- shade(obj, 3)
  } else if (any(unlist(obj$shade) == "white")) {
    obj <- shade(obj, 3)
  } else if (any(unlist(obj$shade) == "black" & sum(grepl("black", unlist(obj$tag))) > 0)) {
    obj <- shade(obj, 1)
  } else if (any(unlist(obj$shade) == "black" & sum(grepl("black", unlist(obj$tag))) == 0)) {
    obj <- shade(obj, 1)
  } else if (any(unlist(obj$shade) == "grey")) {
    obj <- shade(obj, 1)
  } else if (any(unlist(obj$shade) == "grey" )) {
    obj <- shade(obj, 1)
  }
  class(obj) <- "figure"
  return(obj)
}
