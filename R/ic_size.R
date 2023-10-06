#' Size incomplete correlate (Method)
#'
#' Method for drawing the incomplete correlate size distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate size distractor
#' @export ic_size
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-size distractor
#' draw(ic_size(m1))
#' }
ic_size <- function(obj, ...) {
  UseMethod("ic_size")
}

#' Size incomplete correlate
#'
#' Generate incomplete correlate size distractor of a matrix
#'
#' @param obj The matriks
#' @param ... Other arguments
#'
#' @return The incomplete correlate size distractor
#' @export ic_size.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create a matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # draw the matrix
#' draw(m1)
#' # draw the ic-size distractor
#' draw(ic_size(m1))
#' }
ic_size.matriks <- function(obj, ...) {
  m_correct <- correct(obj)
  if (inherits(obj$mat.type, "numeric") == TRUE) {
    dist_ic_size <- size(m_correct, 3)
  } else {
    split_correct <- split_mat(obj)

    if (any(m_correct$tag[[length(m_correct$tag)]] == "compose4" | m_correct$tag[[length(m_correct$tag)]] == "compose2")) {
      index <- unlist(lapply(m_correct$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
      changing<-(length(split_correct)-index[length(index)]+1):length(split_correct)
      transvestite <- which(m_correct$visible==1)
      transvestite <- intersect((length(m_correct$visible)-index[length(index)]+1):(max(transvestite)),transvestite)
      trans<-transvestite-(max(transvestite)-max(changing))
      changing<-intersect(trans,changing)
      new_image<-list()
      for (i in 1:length(changing)) {
        new_image[[i]] <-  size(split_correct[[changing[i]]], 3)

      }
    } else  {
      new_image <- list(size(split_correct[[length(split_correct)]], 3))

      changing<-   sum(m_correct$visible==1)
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
