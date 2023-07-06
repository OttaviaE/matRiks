#' Negative incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_neg
#' @export
#'
#' @examples
ic_neg <- function(obj) {
  UseMethod("ic_neg")
}

#' Negative incomplete correlate
#'
#' Change color to the correct response for creating the ic neg distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_neg.matriks
#' @export
#'
#' @examples
ic_neg.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    if (all(!unlist(m_correct$tag) == "fill")) {
      dist_ic_neg <- margin(m_correct, 2, "lty")
    } else {
      dist_ic_neg <- change_color(m_correct)
    }
  } else {
    split_correct <- split_mat(obj)
    if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
      index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))

      changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)

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
      dist_ic_neg <-  replace(dist_ic_neg,  transvestite[changing[i]],
                              new_image[[i]])
    }

  }

  class(dist_ic_neg) <- "figure"
  return(dist_ic_neg)
}
