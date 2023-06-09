#' Incomplete incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_inc
#' @export
#'
#' @examples
ic_inc <- function(obj) {
  UseMethod("ic_inc")
}

#' incative incomplete correlate
#'
#' Change color to the correct response for creating the ic inc distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_inc.matriks
#' @export
#'
#' @examples
ic_inc.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_inc <- m_correct
    warning("IC-Inc cannot be obtained with a single figure")
  } else {
    split_correct <- split_mat(obj)
    dist_ic_inc <-  hide(m_correct, length(m_correct$shape))

  }

  class(dist_ic_inc) <- "figure"
  return(dist_ic_inc)
}
