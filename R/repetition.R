#' Repetion distractors (Method)
#'
#' Method for drawing the repetition distractors of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list of length 3 with the three repetition distractors
#' @export repetition
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty", vrules = "size")
#' # draw the matrix
#' draw(m1)
#' # draw the repetition distractors
#' draw(repetition(m1))
#' }
repetition <- function(obj, ...) {
  UseMethod("repetition")
}


#' Repetion distractors (Method)
#'
#' Generates the repetition distractors of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list of length 3 with the three repetition distractors
#' @export repetition.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty", vrules = "size")
#' # draw the matrix
#' draw(m1)
#' # draw the repetition distractors
#' draw(repetition(m1))
#' }
repetition.matriks <- function(obj, ...) {
  m_correct = correct(obj)
  n.cell<-obj$mat.type
  if (n.cell == 9) {

    distr_repetition = list(  r_top = obj$Sq6,
                              r_diag = obj$Sq5,
                              r_left = obj$Sq8)
  } else if (n.cell == 4){

    distr_repetition = list(  r_top = obj$Sq2,
                              r_diag = obj$Sq1,
                              r_left = obj$Sq4)
  }

  # I do realize this is not ideal but it works ----

  if (any(unlist(distr_repetition$r_top) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-Top is equal to the correct response")
  }
  if (any(unlist(distr_repetition$r_left) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-left is equal to the correct response")
  }
  if (any(unlist(distr_repetition$r_diag) != unlist(m_correct),
          na.rm = T) == F) {
    warning("R-diag is equal to the correct response")
  }
  class(distr_repetition) <- "responses"
  return(distr_repetition)
}
