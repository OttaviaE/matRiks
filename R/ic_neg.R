#' Negative incomplete correlate (Method)
#'
#' Method for drawing the incomplete correlate negative distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate negative distractor
#' @export ic_neg
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-neg distractor
#' draw(ic_neg(m1))
#' }
ic_neg <- function(obj, ...) {
  UseMethod("ic_neg")
}

#' Negative incomplete correlate
#'
#' Generate incomplete correlate negative distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate negative distractor
#' @export ic_neg.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-neg distractor
#' draw(ic_neg(m1))
#' }
ic_neg.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (inherits(obj$mat.type, "numeric") == TRUE) {
    if (all(!unlist(m_correct$tag) == "fill")) {
      dist_ic_neg <- margin(m_correct, 2, "lty")
    } else {
      dist_ic_neg <- change_color(m_correct)
    }
  } else {
    split_correct <- decof(m_correct)
    if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
      index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
      changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)
      transvestite <- which(m_correct$visible==1)
      transvestite <- intersect((length(m_correct$visible)-index[length(index)]+1):(max(transvestite)),transvestite)
      trans<-transvestite-(max(transvestite)-max(changing))
      changing<-intersect(trans,changing)


      new_image<-list()
      for (i in 1:length(changing)) {
        if (all(!unlist(split_correct[[changing[i]]]) == "fill",na.rm=TRUE)) {
          new_image[[i]] <-  margin(split_correct[[changing[i]]], 2, "lty")
        } else {
          new_image[[i]] <-  change_color(split_correct[[changing[i]]])
        }
      }


    } else if (all(!unlist(split_correct) == "fill",na.rm=TRUE)) {
      new_image <- list(margin(split_correct[[length(split_correct)]], 2, "lty"))
      changing<-length(m_correct$shape)
    } else {
      new_image <- list(change_color(split_correct[[length(split_correct)]]))
      changing<-length(m_correct$shape)
    }

    dist_ic_neg <-m_correct
    transvestite<-which(m_correct$visible==1)
    for(i in 1:length(changing)){
      dist_ic_neg <-  replace(dist_ic_neg,  intersect(transvestite, changing[i]),
                              new_image[[i]])
    }

  }

  class(dist_ic_neg) <- "figure"
  return(dist_ic_neg)
}
