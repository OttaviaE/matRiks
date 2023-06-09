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
  # Vanno messi i warning più che sulla risposta corretta sui wp e sui repetition
  # if (any(unlist(distr_ic$ic_inc) != unlist(m_correct),
  #         na.rm = T) == F) {
  #   warning("IC-inc is equal to the correct response")
  # }
  # if (any(unlist(distr_ic$ic_flip) != unlist(m_correct),
  #         na.rm = T) == F) {
  #   warning("IC-Flip is equal to the correct response")
  # }
  # if (any(unlist(distr_ic$r_diag) != unlist(m_correct),
  #         na.rm = T) == F) {
  #   warning("R-diag is equal to the correct response")
  # }
  class(distr_ic) <- "responses"
  return(distr_ic)
}
