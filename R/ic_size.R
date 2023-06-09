#' size incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_size
#' @export
#'
#' @examples
ic_size <- function(obj) {
  UseMethod("ic_size")
}

#' sizeative incomplete correlate
#'
#' Change color to the correct response for creating the ic size distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_size.matriks
#' @export
#'
#' @examples
ic_size.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_size <- change_color(m_correct)
  } else {
    split_correct <- split_mat(obj)
    dist_ic_size <-  replace(m_correct, length(m_correct$shape),
                             size(split_correct[[length(split_correct)]]))

  }

  class(dist_ic_size) <- "figure"
  return(dist_ic_size)
}
