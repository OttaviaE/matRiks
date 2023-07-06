#' Flip incomplete correlate (method)
#'
#' Method for drawing the incomplete correlate flip distractor of a matrix
#'
#' @param obj Matriks
#'
#' @return The incomplete correlate flip distractor
#' @export ic_flip
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-flip distractor
#' draw(ic_flip(m1))
#' }
ic_flip <- function(obj) {
  UseMethod("ic_flip")
}

#' Flip incomplete correlate
#'
#' Generate incomplete correlate flip distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate flip distractor
#' @export ic_flip.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-flip distractor
#' draw(ic_flip(m1))
#' }
ic_flip.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  the_rules <- c(obj$hrule, obj$vrule)
  if (class(obj$mat.type) == "numeric" & any(grepl("AND|OR", the_rules)) == FALSE) {
    if (any(unlist(m_correct$tag) == "rotate")){
      dist_ic_flip <- rotate(m_correct, 2)
    } else {
      token <- TRUE
      dist_ic_flip <- m_correct
      warning("Can't rotate, sorry!")
    }

  } else {
    split_correct <- split_mat(obj)
    if (any(unlist(split_correct[[length(split_correct)]]$tag) == "rotate")) {
      dist_ic_flip <-  replace(m_correct, length(m_correct$shape),
                               rotate(split_correct[[length(split_correct)]]))
    } else {
      token <- TRUE
      for (i in 1:length(split_correct)) {
        if (any(unlist(split_correct[[i]]$tag) == "rotate") & token == TRUE) {
          dist_ic_flip <- replace(m_correct, i,
                                  rotate(split_correct[[i]], 2), visible = TRUE)
          token <- FALSE
        }
      }

      if (token == TRUE) {

        if (any(unlist(m_correct$tag) == "compose4" | unlist(m_correct$tag) == "compose2")) {
          for (i in 1:length(m_correct$tag)) {
            if (any(m_correct$tag[[i]] == "compose4" | m_correct$tag[[i]] == "compose2") & token == TRUE) {
              index <- as.integer(gsub("compose", "",  m_correct$tag[[i]][grepl("compose", m_correct$tag[[i]])]))
              vis_m_correct <- m_correct$visible[i:(i+index-1)]

              if (index == 4) {
                if (vis_m_correct[2] != vis_m_correct[4]) {
                  vis_m_correct[c(2,4)] <- 1- vis_m_correct[c(2,4)]
                  token <- FALSE
                }
                if (vis_m_correct[1] != vis_m_correct[3]) {
                  vis_m_correct[c(1,3)] <- 1 - vis_m_correct[c(1,3)]
                  token <- FALSE
                }
              } else {
                vis_m_correct <- 1-vis_m_correct
                token <- FALSE
              }
              dist_ic_flip <- m_correct
              dist_ic_flip$visible[i:(i+index-1)] <- vis_m_correct

              # if (sum(vis_m_correct) != index & sum(vis_m_correct) != 0 ) {
              #   names_m_correct <- m_correct$shape[i:(1+index-1)]
              #   names_m_correct <- names_m_correct[vis_m_correct == 1]
              # }
          }
          }
        }

        if (token == TRUE) {
          dist_ic_flip <- m_correct
          warning("Can't rotate, sorry!")
        }



      }

    }

  }

  class(dist_ic_flip) <- "figure"
  return(dist_ic_flip)
}
