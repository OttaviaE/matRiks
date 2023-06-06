#' Method for changing color
#'
#' @param obj The figure
#' @param ... other arguments
#'
#' @return l
#' @export change_color
#' @export
#'
#' @examples
change_color <- function(obj, ...) {
  UseMethod("change_color")
}


#' Change color
#'
#' @param obj The figure
#' @param ... other arguments
#'
#' @return
#' @export change_color.figure
#' @export
#'
#' @examples
change_color.figure <- function(obj, ...) {
  if (any(is.na(unlist(obj$shade))) == TRUE) {
    obj = shade(obj, 3)
  } else if (any(unlist(obj$shade) == "white")) {
    obj = shade(obj, 3)
  } else if (any(unlist(obj$shade) == "black" & sum(grepl("black", unlist(obj$tag))) > 0)) {
    obj = shade(obj, 2)
  } else if (any(unlist(obj$shade) == "black" & sum(grepl("black", unlist(obj$tag))) == 0)) {
    obj = shade(obj, 2)
  }
  class(obj) <- "figure"
  return(obj)
}
