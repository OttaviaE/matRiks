#' Negative incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export repetition
#' @export
#'
#' @examples
ic_neg <- function(obj) {
  UseMethod("ic_neg")
}

#' Negative incomplete correlate
#'
#' Change color to the correct response for creating the ic neg distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_neg.matriks
#' @export
#'
#' @examples
ic_neg.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_neg <- change_color(m_correct)
  } else {
    split_correct <- split_mat(obj)
   dist_ic_neg <-  replace(m_correct, length(m_correct$shape),
            change_color(split_correct[[length(split_correct)]]))

  }

  class(dist_ic_neg) <- "figure"
  return(dist_ic_neg)
}
