
#' Repetition (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export repetition
#' @export
#'
#' @examples
repetition <- function(obj) {
  UseMethod("repetition")
}


#' Repetition distractors
#'
#' Generates the repetition distractors
#'
#' @param obj object of class obj
#' @param ... Other arguments
#'
#' @return A list of length three
#' @export repetition.matriks
#' @export
#'
#' @examples
repetition.matriks <- function(obj, ...) {
  m_correct = correct(obj)

  if (obj$mat.type == 9) {

    distr_repetition = list(  r.top = obj$Sq6,
                              r.diag = obj$Sq5,
                              r.left = obj$Sq8)
  } else if (obj$mat.type == 4){

    distr_repetition = list(  r.top = obj$Sq2,
                              r.diag = obj$Sq1,
                              r.left = obj$Sq4)
  }

  # I do realize this is not ideal but it works ----

  if (any(unlist(distr_repetition$r.top) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-Top is equal to the correct response")
  }
  if (any(unlist(distr_repetition$r.left) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-left is equal to the correct response")
  }
  if (any(unlist(distr_repetition$r.diag) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-diag is equal to the correct response")
  }
  return(distr_repetition)
}
