#' Flip incomplete correlate (method)
#'
#' Method for drawing the incomplete correlate flip distractor of a matrix
#'
#' @param obj Matriks
#' @param ... other arguments
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
ic_flip <- function(obj, ...) {
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
  split_correct <- split_mat(obj)
  the_rules <- c(obj$hrule, obj$vrule)
  if (inherits(obj$mat.type, "numeric") == TRUE & any(grepl("AND|OR", the_rules)) == FALSE) {
    if (any(unlist(m_correct$tag) == "rotate")){
      dist_ic_flip <- rotate(m_correct, 2)
    } else if (inherits(obj$mat.type, "numeric") == TRUE  & any(grepl("bow_tie", unlist(split_correct))) == TRUE) {
      if (any(unlist(m_correct$tag) == "simple") == TRUE) {
        size_x <- (m_correct$size.x)
        size_y <- (m_correct$size.y)
        lty_fig <- (m_correct$lty)
        lwd_fig <- (m_correct$lwd)
        shd_fig <- (m_correct$shade)
        if (any(grepl("vert", unlist(m_correct$tag))) == TRUE) {
          replace_figure <- s_hor_bow_tie()
          pos_x <- list(c(m_correct$pos.y[[1]][1], m_correct$pos.y[[1]][2]))
          pos_y <- list(c(0,0))
        } else {
          replace_figure <- s_vert_bow_tie()
          pos_x <- list(c(0,0))
          pos_y <- list(c(m_correct$pos.x[[1]][1], m_correct$pos.x[[1]][2]))
        }
      } else {
        size_x <- list(m_correct$size.x[[1]], m_correct$size.x[[2]])
        size_y <- list(m_correct$size.y[[1]], m_correct$size.y[[2]])
        lty_fig <- list(m_correct$lty[[1]], m_correct$lty[[2]])
        lwd_fig <- list(m_correct$lwd[[1]], m_correct$lwd[[2]])
        shd_fig <-  list(m_correct$shade[[1]], m_correct$shade[[2]])
        if (any(unlist(m_correct$tag) == "vert")) {
          replace_figure <- hor_bow_tie()
          pos_x <- (m_correct$pos.y)
          pos_y <- list(0, 0)
        } else {
          replace_figure <- vert_bow_tie()
          pos_y <- (m_correct$pos.x)
          pos_x <- list(0, 0)
        }
      }

      replace_figure$size.x[c(1,2)]  <- size_x
      replace_figure$size.y[c(1,2)]  <- size_y
      replace_figure$lty[c(1,2)]  <- lty_fig
      replace_figure$lwd[c(1,2)]  <- lwd_fig
      replace_figure$shade[c(1,2)]  <- shd_fig
      replace_figure$pos.x[c(1,2)]  <- pos_x
      replace_figure$pos.y[c(1,2)]  <- pos_y
      dist_ic_flip <- replace_figure

    } else {
      token <- TRUE
      dist_ic_flip <- cof(m_correct, size(X(lwd = 10), 3, "inv"))
      warning("Can't rotate, sorry!")
    }

  } else {
    if (any(unlist(split_correct[[length(split_correct)]]$tag) == "rotate") & all(grepl("bow_tie", unlist(split_correct)) == FALSE)) {
      dist_ic_flip <-  replace(m_correct, max(which(m_correct$visible==1)),
                               rotate(split_correct[[length(split_correct)]]))
    } else if (!all(grepl("bow_tie", unlist(split_correct)) == FALSE)){
      token <- TRUE
      index_figure <- which(grepl("bow_tie", m_correct$tag))

      if (any(unlist(m_correct$tag[[index_figure]]) == "simple") == TRUE) {
        size_x <- list(m_correct$size.x[[index_figure]][1], m_correct$size.x[[index_figure]][2])
        size_y <- list(m_correct$size.y[[index_figure]][1], m_correct$size.y[[index_figure]][2])
        lty_fig <- list(m_correct$lty[[index_figure]][1], m_correct$lty[[index_figure]][2])
        lwd_fig <- list(m_correct$lwd[[index_figure]][1], m_correct$lwd[[index_figure]][2])
        shd_fig <- list(m_correct$shade[[index_figure]][1], m_correct$shade[[index_figure]][2])
        if (any(grepl("vert", unlist(m_correct$tag))) == TRUE) {
          replace_figure <- hor_bow_tie()
          pos_x <- list(m_correct$pos.y[[index_figure]][1], m_correct$pos.y[[index_figure]][2])
          pos_y <- list(0,0)
        } else {
          replace_figure <- vert_bow_tie()
          pos_x <- list(0,0)
          pos_y <- list(m_correct$pos.x[[index_figure]][1], m_correct$pos.x[[index_figure]][2])
        }
      } else {
        size_x <- list(m_correct$size.x[[index_figure]], m_correct$size.x[[index_figure+1]])
        size_y <- list(m_correct$size.y[[index_figure]], m_correct$size.y[[index_figure+1]])
        lty_fig <- list(m_correct$lty[[index_figure]], m_correct$lty[[index_figure+1]])
        lwd_fig <- list(m_correct$lwd[[index_figure]], m_correct$lwd[[index_figure+1]])
        shd_fig <- list(m_correct$shade[[index_figure]], m_correct$shade[[index_figure+1]])
        if (any(unlist(m_correct$tag) == "vert")) {
          replace_figure <- hor_bow_tie()
          pos_x <- list(m_correct$pos.y[[index_figure]], m_correct$pos.y[[index_figure+1]])
          pos_y <- list(0, 0)
        } else {
          replace_figure <- vert_bow_tie()
          pos_y <- list(m_correct$pos.x[[index_figure]], m_correct$pos.x[[index_figure+1]])
          pos_x <- list(0, 0)
        }
      }

      # fa cagare ma non mi funziona il cervello scusa
      replace_figure$size.x[c(1,2)] <- size_x
      replace_figure$size.y[c(1,2)] <- size_y
      replace_figure$lty[c(1,2)] <- lty_fig
      replace_figure$lwd[c(1,2)] <- lwd_fig
      replace_figure$shade[c(1,2)] <- shd_fig
      replace_figure$pos.x[c(1,2)] <- pos_x
      replace_figure$pos.y[c(1,2)] <- pos_y




      if (any(grepl("compose2", split_correct[[index_figure]])) == TRUE) {

        my_figure <- replace(m_correct, index_figure, split_mat(replace_figure)[[1]])
        my_figure <- replace(my_figure, index_figure +1, split_mat(replace_figure)[[2]])
        dist_ic_flip <- my_figure
      } else {
        new_replace <- cof(split_mat(replace_figure)[[1]], split_mat(replace_figure)[[2]],
                           single = TRUE, name = "single_bow")
        dist_ic_flip <- replace(m_correct, index_figure,
                                new_replace)
      }

      token <- FALSE
    } else {
      token <- TRUE

      for (i in length(split_correct):1) {

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
          dist_ic_flip <- cof(m_correct, size(X(lwd = 10), 3, "inv"))
          warning("Can't rotate, sorry!")
        }



      }

    }

  }
  class(dist_ic_flip) <- "figure"
  return(dist_ic_flip)
}
