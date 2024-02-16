#' Split the correct response (Method)
#'
#' Split all the visible figures composing a cell of the matrix or of a concatenation of figures
#'
#' @param obj The complex figure or the matrix to split
#' @param vis logical, split only the visible figures. Default is TRUE
#' @param cell integer, The index of the cell to be split. Default is the correct response, either cell 4 (4-cell matriks) or cell 9 (9-cell matriks)
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures composing the complex figure (vis = FALSE)
#'
#' @export split_mat
#' @export
#'
#' @examples
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1$Sq1)
split_mat<- function(obj, vis = TRUE, cell = NULL) {
  UseMethod("split_mat")
}
#' @describeIn split_mat Split the correct response
#'
#' Split all the visible figures composing a cell of the matrix or of a concatenation of figures
#'
#' @param obj The complex figure or the matrix to split
#' @param vis logical, split only the visible figures. Default is TRUE
#' @param cell integer, The index of the cell to be split. Default is the correct response, either cell 4 (4-cell matriks) or cell 9 (9-cell matriks)
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures composing the complex figure (vis = FALSE)
#'
#' @export split_mat.figure
#' @export
#'
#' @examples
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1$Sq1)
split_mat.figure = function(obj, vis = TRUE, cell = NULL) {

  if(vis == TRUE) {
   #index_elements<-which(obj$visible==1 & unlist(lapply(obj$num, function(x,y) all(x==y), 1)))
    index_elements<-which(obj$visible==1)
  } else {
    index_elements <- 1:length(obj$shape)
  }
  split_m <- vector("list", length(index_elements))
  for (i in 1:length(split_m)) {
    split_m[[i]] <- vector("list", length(obj))
    for (j in 1:length(split_m[[i]])) {
      names(split_m)[i] = obj$shape[index_elements[i]]
      attr(split_m[[i]], "class") = "figure"
      split_m[[i]][[j]] = obj[[j]][index_elements[i]]
      names(split_m[[i]])[j] = names(obj)[j]
    }
  }
 if(length(obj$tag)<max(index_elements))
 {
   #Va completato
   # i don't what I'm doing daje
   tag_index <- unlist(lapply(obj$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
   if (length(tag_index) == 1) {
     index <- which(grepl("compose", obj$tag)) # forse non senso
     changing<-(length(split_m)-tag_index[length(tag_index)]+1):length(split_m)
     the_tag <- obj$tag[[index]]
     transvestite <- which(obj$visible==1)
     transvestite <- intersect((length(obj$visible)-tag_index[length(tag_index)]+1):(max(transvestite)),transvestite)
     trans<-transvestite-(max(transvestite)-max(changing))
     figure_index<-intersect(trans,changing)

    # the_tag <- the_tag[-grep("compose", the_tag)]
    # figure_index <- (length(obj$shape) - object_index +1):((length(obj$shape)-object_index)+tag_index-1)
     for (i in figure_index) {
       split_m[[i]]$tag[[1]] <- the_tag
     }
   } else {
     the_tag <- list()
     for (i in 1:length(tag_index)) {
       index <- which(grepl("compose", obj$tag)) # forse non senso
       the_tag[[i]] <- obj$tag[[index[i]]]
       changing<-(length(split_m)-tag_index[length(tag_index)]+1):length(split_m)
       transvestite <- which(obj$visible==1)
       transvestite <- intersect((length(obj$visible)-tag_index[length(tag_index)]+1):(max(transvestite)),transvestite)
       trans<-transvestite-(max(transvestite)-max(changing))
       figure_index<-intersect(trans,changing)

       # the_tag <- the_tag[-grep("compose", the_tag)]
       # figure_index <- (length(obj$shape) - object_index +1):((length(obj$shape)-object_index)+tag_index-1)
       for (j in figure_index) {
         split_m[[j]]$tag[[1]] <- unlist(the_tag[[i]])
       }
      # the_tag[[i]] <- the_tag[[i]][-grep("compose", the_tag[[i]])]
       # figure_index <- (length(obj$shape) - tag_index +1):((length(obj$shape)-tag_index)+tag_index-1)
       # for (j in figure_index) {
       #   obj$tag[[j]] <- unlist(the_tag[[i]]) # se non funziona è perché questo codice ha dei problemi, la soluzione probabilmente è sostitutire obj con split_m (Andrea, 12/10/2023)
       # }
     }
   }

 }

  return(split_m)
}
#' @describeIn split_mat Split all the visible figures composing a cell of the matrix or a concatenation of figures
#'
#' @param obj The complex figure or the matrix to split
#' @param vis logical, split only the visible figures. Default is TRUE
#' @param cell integer, The index of the cell to be split. Default is the correct response
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures composing the complex figure (vis = FALSE)
#' @export split_mat.matriks
#' @export
#'
#' @examples
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1)
split_mat.matriks = function(obj, vis = TRUE, cell = NULL) {

  if (is.null(cell) == TRUE) {
    cell.start = correct(obj)
  } else {
    cell = paste0("Sq", cell)
    cell.start = obj[[cell]]
  }
  split_m<-split_mat(cell.start, vis = vis)
  return(split_m)
}

#' Correct response (Method)
#'
#' Isolate the correct response from a matriks
#'
#' @param obj The matriks
#'
#' @return The correct response of a matriks
#'
#' @export correct
#' @export
#'
#' @examples
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat <- mat_apply(triangle(), mat.type = 9, hrule = "size")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
#'
#' # apply the rotate rule on a pacman for creating a matriks with 4 cells
#' my_mat <- mat_apply(pacman(), mat.type = 4,
#'                   vrule = "rotate")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
correct<- function(obj) {
  UseMethod("correct")
}
#' @describeIn correct Correct response
#'
#' Isolate the correct response from a matriks
#'
#' @param obj The matrix
#' @return The correct response of a matriks
#' @export correct.matriks
#' @export
#'
#' @examples
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat <- mat_apply(triangle(), mat.type = 9, hrule = "size")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
#'
#' # apply the rotate rule on a pacman for creating a matriks with 4 cells
#' my_mat <- mat_apply(pacman(), mat.type = 4,
#'                   vrule = "rotate")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
correct.matriks = function(obj) {

    n.cell<-obj$mat.type
  if (n.cell == 9) {
    correct <- obj$Sq9
  } else {
    correct <- obj$Sq4
  }
  return(correct)
}

