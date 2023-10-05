#' Repetion distractors (Method)
#'
#' Method for drawing the repetition distractors of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list of length 3 with the three repetition distractors, R-top, R-left, R-diag
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


#' Repetition distractors
#'
#' Generates the repetition distractors of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list of length 3 with the three repetition distractors, R-top, R-left, R-diag
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

    distr_repetition <- list(  r_top = obj$Sq6,
                              r_diag = obj$Sq5,
                              r_left = obj$Sq8)
  } else if (n.cell == 4){

    distr_repetition <- list( r_top = obj$Sq2,
                              r_diag = obj$Sq1,
                              r_left = obj$Sq3)
  }

  # I do realize this is not ideal but it works ----

  if (all(unlist(distr_repetition$r_top) == unlist(m_correct),
          na.rm = TRUE) == TRUE) {
    warning("R-Top is equal to the correct response")
    distr_repetition$r_top <- cof(distr_repetition$r_top,
                                  size(X(lwd = 10), 3, "inv"))
  }
  if (all(unlist(distr_repetition$r_left) == unlist(m_correct),
          na.rm = TRUE) == TRUE) {
    warning("R-left is equal to the correct response")
    distr_repetition$r_left <- cof(distr_repetition$r_left,
                                  size(X(lwd = 10), 3, "inv"))
  }
  if (all(unlist(distr_repetition$r_diag) == unlist(m_correct),
          na.rm = TRUE) == TRUE) {
    warning("R-diag is equal to the correct response")
    distr_repetition$r_diag <- cof(distr_repetition$r_diag,
                                  size(X(lwd = 10), 3, "inv"))
  }
  class(distr_repetition) <- "responses"
  return(distr_repetition)
}
