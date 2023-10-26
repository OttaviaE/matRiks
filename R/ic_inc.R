#' @describeIn ic Incomplete correlate incomplete distractor (method)
#'
#' Generate incomplete correlate incomplete distractor from a matriks
#'
#' @inheritParams ic_flip
#'
#' @return An object of class figure that is the incomplete correlate incomplete distractor of a matrix. If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic_inc
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' m2 <- mat_apply(dot(), "shade")
#' mat <- com(m1, m2)
#' # draw the matrix
#' draw(mat)
#' # draw the incomplete correlate incomplete distractor
#' draw(ic_inc(mat))
ic_inc <- function(obj, ...) {
  UseMethod("ic_inc")
}
#' @describeIn ic Incomplete correlate incomplete distractor
#'
#' Generate incomplete correlate incomplete distractor from a matriks
#'
#' @inheritParams ic_inc
#'
#' @return An object of class figure that is the incomplete correlate incomplete distractor of a matrix. If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic_inc.matriks
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' m2 <- mat_apply(dot(), "shade")
#' mat <- com(m1, m2)
#' # draw the matrix
#' draw(mat)
#' # draw the incomplete correlate incomplete distractor
#' draw(ic_inc(mat))
ic_inc.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (inherits(obj$mat.type, "numeric") == TRUE & any(grepl("compose", m_correct$tag) == FALSE) | sum(m_correct$visible) == 1) {
    dist_ic_inc <- cof(m_correct, size(X(lwd = 10), 3, "inv"))
    warning("IC-Inc cannot be obtained with a single figure")
  } else {
    #split_correct <- split_mat(obj)
    #dist_ic_inc <-  hide(m_correct, length(m_correct$shape))
    transvestite <- which(m_correct$visible==1)
    dist_ic_inc <-  hide(m_correct, max(transvestite))
  }

  class(dist_ic_inc) <- "figure"
  return(dist_ic_inc)
}
