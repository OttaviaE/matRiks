#' Incomplete correlate distractors (method)
#'
#' Generate incomplete correlate flip distractor from a matriks
#'
#' @inheritParams ic_inc
#'
#' @return An object of class responses of length 4, which contains the incomplete correlate distractors of a matriks (IC-Inc, IC-Flip, IC-Neg, IC-Size). . If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic
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
#' draw(ic(m1))
ic <- function(obj) {
  UseMethod("ic")
}


#' @describeIn ic Incomplete correlate distractors
#'
#' Generate incomplete correlate flip distractor from a matriks
#'
#' @inheritParams ic_inc
#'
#' @return An object of class responses of length 4, which contains the incomplete correlate distractors of a matriks. If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic
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
#' draw(ic(m1))
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
