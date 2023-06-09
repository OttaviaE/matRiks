#' Response list (method)
#'
#' @param obj the matrix
#' @param seed the seed
#' @param ...  other arguments
#'
#' @return A list
#' @export response_list
#' @export
#'
#' @examples
response_list <-function(obj, seed, ...) {
  UseMethod("response_list")
}

#' Response list
#'
#' @param obj the matrix
#' @param seed the seed
#' @param ...  other arguments
#'
#' @return A list
#' @export response_list.matriks
#' @export
#'
#' @examples
response_list.matriks <- function(obj, seed = 666, ...) {
  my_repetion <- repetition(obj)
  my_wp <- wp(obj)
  resp <- list(correct = correct(obj),
               r_diag = my_repetion$r_diag,
               r_left = my_repetion$r_left,
               r_top = my_repetion$r_top,
               wp_copy = my_wp$wp_copy,
               wp_matrix = my_wp$wp_matrix,
               difference = difference(obj, seed = seed),
               ic_neg = ic_neg(obj),
               ic_flip = ic_flip(obj),
               ic_size = ic_size(obj),
               ic_inc = ic_inc(obj) )
  class(resp) <- "responses"
  return(resp)
}
