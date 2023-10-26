#' @describeIn ic Incomplete correlate negative distractor (method)
#'
#' Generate incomplete negative incomplete distractor from a matriks
#'
#' @inheritParams ic_inc
#'
#' @return An object of class figure that is the incomplete correlate negative distractor of a matrix. If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic_neg
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the incomplete correlate negative distractor
#' draw(ic_neg(m1))
ic_neg <- function(obj, ...) {
  UseMethod("ic_neg")
}
#' @describeIn ic Incomplete correlate negative distractor
#'
#' Generate incomplete negative incomplete distractor from a matriks
#'
#' @inheritParams ic_inc
#'
#' @return An object of class figure that is the incomplete correlate negative distractor of a matrix. If the distractor could not be generated because of the constraints imposed by the matrix, it will be covered by a thick, black X and a warning is given.
#' @export ic_neg.matriks
#' @export
#'
#' @examples
#' # create a matrix
#' m1 <- mat_apply(pacman(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the incomplete correlate negative distractor
#' draw(ic_neg(m1))
ic_neg.matriks <- function(obj, ...) {

  w <- FALSE
  m_correct <- correct(obj)
  if (inherits(obj$mat.type, "numeric") == TRUE) {
    if (all(!unlist(m_correct$tag) == "fill")) {
      w <- TRUE
      dist_ic_neg <- cof(m_correct, size(X(lwd = 10), 3, "inv"))
      warning("Can't change color, sorry!")
    } else {
      dist_ic_neg <- change_color(m_correct)
    }
  } else {
    # split_correct <- decof(m_correct)
    # if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
    #   index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
    #   changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)
    #   transvestite <- which(m_correct$visible==1)
    #   transvestite <- intersect((length(m_correct$visible)-index[length(index)]+1):(max(transvestite)),transvestite)
    #   trans<-transvestite-(max(transvestite)-max(changing))
    #   changing<-intersect(trans,changing)
    split_correct <- split_mat(obj)

    if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
      index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
      changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)
      transvestite <- which(m_correct$visible==1)
      transvestite <- intersect((length(m_correct$visible)-index[length(index)]+1):(max(transvestite)),transvestite)
      trans<-transvestite-(max(transvestite)-max(changing))
      changing<-intersect(trans,changing)

#
#       new_image<-list()
#       for (i in 1:length(changing)) {
#         if (all(!unlist(split_correct[[changing[i]]]) == "fill",na.rm=TRUE)) {
#           new_image[[i]] <-  margin(split_correct[[changing[i]]], 2, "lty")
#         } else {
#           new_image[[i]] <-  change_color(split_correct[[changing[i]]])
#         }
#       }
      new_image<-list()
      for (i in 1:length(changing)) {
        if (all(!unlist(split_correct[[changing[i]]]$tag) == "fill",na.rm=TRUE)) {
          w <- TRUE
          warning("Can't change color, sorry!")
          break
        } else {
        new_image[[i]] <-  change_color(split_correct[[changing[i]]])
        }
      }
    } else {
      split_correct <- split_mat(m_correct)
      changing <- sum(m_correct$visible)
      transvestite<-which(m_correct$visible==1)
      if (all(!unlist(split_correct[[changing]]$tag) == "fill",na.rm=TRUE)) {
        w <- TRUE
        warning("Can't change color, sorry!")
      } else {
      new_image <- list(change_color(split_correct[[changing]]))
    }
}

    if (w == TRUE) {
      dist_ic_neg <- cof(m_correct, size(X(lwd = 10), 3, "inv"))
    } else {
      dist_ic_neg <-m_correct
      for(i in 1:length(changing)){
        transvestite<-which(m_correct$visible==1)
        dist_ic_neg <-  replace(dist_ic_neg,
                                #intersect(transvestite, changing[i]),
                                transvestite[changing[i]],
                                new_image[[i]])
      }
    }


  }
  class(dist_ic_neg) <- "figure"
  return(dist_ic_neg)
  }
