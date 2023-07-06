#' Incomplete correlate (method)
#'
#' Method for drawing the incomplete correlate distractors of a matrix
#'
#' @param obj Matriks
#'
#' @return A list of incomplete correlate distractors to be passed to the draw function
#' @export ic
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic distractor
#' draw(ic(m1))
#' }
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
#' @return A list of length 3 composed of the incomplete correlate distractors to be passed to the draw function
#' @export ic.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic distractor
#' draw(ic(m1))
#' }
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
