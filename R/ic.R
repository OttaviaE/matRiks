#' Incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic
#' @export
#'
#' @examples
ic <- function(obj) {
  UseMethod("ic")
}


#' Incomplete correlate distractors
#'
#' Generates the incomplete correlate distractors
#'
#' @param obj object of class obj
#' @param ... Other arguments
#'
#' @return A list of length three
#' @export ic.matriks
#' @export
#'
#' @examples
ic.matriks <- function(obj, ...) {
  m_correct = correct(obj)

  distr_ic <- list(
    ic_inc = ic_inc(obj),
    ic_flip = ic_flip(obj),
    ic_neg = ic_neg(obj),
    ic_size = ic_size(obj)
  )

  class(distr_ic) <- "responses"
  return(distr_ic)
}
