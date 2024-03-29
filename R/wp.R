#' Wrong principle distractors (method)
#'
#' Generate the wrong principle distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return An object of class responses that contains the wrong principle distractors of a matriks (WP-Matrix and WP-Copy). If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export wp
#' @export
#'
#' @examples
#' m1 <- mat_apply(hexagon(),  hrules = "lty")
#' # draw the matriks
#' draw(m1)
#' # draw the wp distractors with the title
#' draw(wp(m1), main = TRUE)
wp <- function(obj, ...) {
  UseMethod("wp")
}

#' @describeIn wp Wrong principle distractors
#'
#' Generate the wrong principle distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return An object of class responses that contains the wrong principle distractors of a matriks (WP-Matrix and WP-Copy). If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export wp.matriks
#' @export
#'
#' @examples
#' m1 <- mat_apply(hexagon(),  hrules = "lty")
#' # draw the matriks
#' draw(m1)
#' # draw the wp distractors with the title
#' draw(wp(m1), main = TRUE)
wp.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  hrules <- gsub(".inv", "", obj$hrule)
  vrules <- gsub(".inv", "", obj$vrule)

  if (any(hrules[order(hrules)] == vrules[order(vrules)]) == TRUE) {
    if (obj$mat.type == 4) {
      wp_copy <- obj$Sq1
      wp_matrix_start <- size(obj$Sq3)
    } else {
      wp_copy <- obj$Sq3
      wp_matrix_start <- size(obj$Sq8)
    }
  } else {
    if (obj$mat.type == 4) {
      wp_matrix_start <- size(obj$Sq2)
    } else {
      wp_matrix_start <- size(obj$Sq5)
    }
    wp_copy <- obj$Sq1
  }

  if (length(unlist(wp_copy$nv)) == sum(grepl("rotate", unlist(wp_copy$tag)))) {
    wp_matrix <- cof(wp_matrix_start,
                    rotate(wp_copy))
  } else {
    wp_matrix <- cof(wp_copy,
                    wp_matrix_start)
  }


  distr_wp = list(wp_copy = wp_copy,
                  wp_matrix = wp_matrix)
  class(distr_wp) <- "responses"
  return(distr_wp)

}
