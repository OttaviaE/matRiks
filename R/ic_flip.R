#' Flip incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_flip
#' @export
#'
#' @examples
ic_flip <- function(obj) {
  UseMethod("ic_flip")
}

#' flipative incomplete correlate
#'
#' Change color to the correct response for creating the ic flip distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_flip.matriks
#' @export
#'
#' @examples
ic_flip.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  the_rules <- c(obj$hrule, obj$vrule)
  if (class(obj$mat.type) == "numeric" & any(grepl("AND|OR", the_rules)) == FALSE) {
    dist_ic_flip <- rotate(m_correct, 3)
  } else {
    split_correct <- split_mat(obj)
    if (any(split_correct[[length(split_correct)]]$tag == "rotate")) {
      dist_ic_flip <-  replace(m_correct, length(m_correct$shape),
                               rotate(split_correct[[length(split_correct)]]))
    } else {
      token <- TRUE
      for (i in 1:length(split_correct)) {
        if (any(unlist(split_correct[[i]]$tag) == "rotate") & token == TRUE) {
          dist_ic_flip <- replace(m_correct, i,
                                  rotate(split_correct[[i]], 3), visible = TRUE)
          token <- FALSE
        }
      }
      if (token == TRUE) {
        dist_ic_flip <- m_correct
        warning("Bro non ruota stacce")
      }

    }

  }

  class(dist_ic_flip) <- "figure"
  return(dist_ic_flip)
}
