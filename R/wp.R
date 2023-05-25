#' Wp (method)
#'
#' generate wrong principle distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return a list of two
#' @export wp
#' @export
#'
#' @examples
wp <- function(obj, ...) {
  UseMethod("wp")
}

#' wp
#'
#' generate wrong principle distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return a list of two
#' @export wp.matriks
#' @export
#'
#' @examples
wp.matriks <- function(obj, ...) {
  m_correct = correct(obj)
  hrules =gsub(".inv", "", obj$hrule)
  vrules = gsub(".inv", "", obj$vrule)

  if (any(hrules[order(hrules)] == vrules[order(vrules)]) == TRUE) {
    wp_copy = obj$Sq3
    wp_matrix_start = size(obj$Sq8)
  } else {
    wp_copy = obj$Sq1
    wp_matrix_start = size(obj$Sq5)
  }

  if (any(unlist(wp_copy$tag) =="rotate") == FALSE) {
    wp_matrix = cof(wp_matrix_start,
                    wp_copy)
  } else {
    wp_matrix = cof(wp_matrix_start,
                    rotate(wp_copy))
  }

  distr_wp = list(wp_copy = wp_copy,
                  wp_matrix = wp_matrix)
  return(distr_wp)

}
