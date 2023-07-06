#' size incomplete correlate (method)
#'
#' @param obj Matriks
#'
#' @return A list
#' @export ic_size
#' @export
#'
#' @examples
ic_size <- function(obj) {
  UseMethod("ic_size")
}

#' sizeative incomplete correlate
#'
#' Change color to the correct response for creating the ic size distractors
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return A list
#' @export ic_size.matriks
#' @export
#'
#' @examples
ic_size.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (class(obj$mat.type) == "numeric") {
    dist_ic_size <- size(m_correct, 3)
  } else {
    split_correct <- split_mat(obj)

    if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
      index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))

      changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)

      new_image<-list()
      for (i in 1:length(changing)) {
          new_image[[i]] <-  size(split_correct[[changing[i]]], 3)

      }
    } else if (all(!unlist(split_correct) == "fill",na.rm=TRUE)) {
      new_image <- list(size(split_correct[[length(split_correct)]], 3))
      changing<-length(m_correct$shape)
    }

    dist_ic_size <-m_correct
    transvestite<-which(m_correct$visible==1)
    for(i in 1:length(changing)){
      dist_ic_size <-  replace(dist_ic_size,  transvestite[changing[i]],
                              new_image[[i]])
    }

    #
    # dist_ic_size <-  replace(m_correct, length(m_correct$shape),
    #                          size(split_correct[[length(split_correct)]]), 3)

  }

  class(dist_ic_size) <- "figure"
  return(dist_ic_size)
}
