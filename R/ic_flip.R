#' Flip incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_flip
#' @export
#'
#' @examples
ic_flip <- function(obj) {
  UseMethod("ic_flip")
}

#' flipative incomplete correlate
#'
#' Change color to the correct response for creating the ic flip distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_flip.matriks
#' @export
#'
#' @examples
ic_flip.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_flip <- change_color(m_correct)
  } else {
    split_correct <- split_mat(obj)
    dist_ic_flip <-  replace(m_correct, length(m_correct$shape),
                            rotate(split_correct[[length(split_correct)]]))

  }

  class(dist_ic_flip) <- "figure"
  return(dist_ic_flip)
}
