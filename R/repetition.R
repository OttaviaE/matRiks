#' Repetition distractors (Method)
#'
#' Generate repetition distractors from a matriks
#'
#' @inheritParams ic
#'
#' @return An object of class responses of length 3, which contains the repetition distractors of a matriks (R-Left, R-Top, R-Diag). If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export repetition
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' m2 <- mat_apply(dot(), "shade")
#' mat <- com(m1, m2)
#' # draw the matrix
#' draw(mat)
#' # draw the incomplete correlate distractors
#' draw(repetition(m1))
repetition <- function(obj, ...) {
  UseMethod("repetition")
}
#' @describeIn repetition Repetition distractors (Method)
#'
#' Generate repetition distractors from a matriks
#'
#' @inheritParams ic
#'
#' @return An object of class responses of length 3, which contains the repetition distractors of a matriks (R-Left, R-Top, R-Diag). If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export repetition
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' m2 <- mat_apply(dot(), "shade")
#' mat <- com(m1, m2)
#' # draw the matrix
#' draw(mat)
#' # draw the incomplete correlate distractors
#' draw(repetition(m1))
repetition <- function(obj, ...) {
  UseMethod("repetition")
}
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
