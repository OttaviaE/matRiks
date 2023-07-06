
#' Flip incomplete correlate (Method)
#'
#' Method for drawing the incomplete correlate incomplete distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate incomplete distractor
#' @export ic_inc
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-inc distractor
#' draw(ic_inc(m1))
#' }
ic_inc <- function(obj) {
  UseMethod("ic_inc")
}

#' Flip incomplete correlate
#'
#' Generate incomplete correlate incomplete distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate incomplete distractor
#' @export ic_inc.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-inc distractor
#' draw(ic_inc(m1))
#' }
ic_inc.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_inc <- m_correct
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
