#' Split mat
#'
#' @param obj The figure
#' @param cell the index of the cell to be split
#' @param vis Does the selection involve ony the figures visible in the selected cell? Default is TRUE
#'
#' @export split_mat
#' @export
#'
#' @examples
split_mat<- function(obj,vis = TRUE, cell = NULL) {
  UseMethod("split_mat")
}
#' Isolate the objects in a cell
#'
#' @param obj The figure
#' @param cell the index of the cell to be split
#' @param vis Does the selection involve ony the figures visible in the selected cell? Default is TRUE
#'
#' @return
#' @export split_mat.figure
#' @export
#'
#' @examples
#'
split_mat.figure = function(obj, vis = TRUE, cell = NULL) {
  if(vis == TRUE) {
    index_elements<-which(obj$visible==1 & unlist(lapply(obj$num, function(x,y) all(x==y), 1)))
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
  return(split_m)
}
#' Isolate the visible objects in a cell of the matrix
#'
#' @param obj The matrix
#' @param cell The index of the cell to be splitted. Default is the cell of the correct response
#' @param vis Whether you want to split only the visible figures in a cell (vis = TRUE, default) or all the figures (vis = FALSE)
#'
#' @return A list of the same length of the visible figures in the selected cell (vis = TRUE) or of the same lenght of all the figures in the matrix (vis = TRUE)
#' @export spli_mat.matriks
#' @export
#'
#' @examples
split_mat.matriks = function(obj, vis = TRUE, cell = NULL) {
  if (is.null(cell) == T) {
    cell.start = correct(obj)
  } else {
    cell = paste0("Sq", cell)
    cell.start = obj[[cell]]
  }
  split_m<-split_mat(cell.start, vis = vis)
  return(split_m)
}

#' Correct cell
#'
#' @param obj The matrix
#'
#' @export correct
#' @export
correct<- function(obj) {
  UseMethod("correct")
}
#' Correct response
#'
#' Extract the cell of the correct response, either Sq9 (9-cell matriks) or Sq4 (4-cell matriks)
#'
#' @param obj The matriks
#'
#' @return The cell with the correct response
#' @export correct.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat = mat_apply(triangle(), mat.type = 9,
#' hrule = "size")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
#'
#' # # apply the reflect rule on a pacman for creating a matriks with 4 cell
#' my_mat = mat_apply(pacman(), mat.type = 4,
#'                   vrule = "reflect")
#' # draw the matriks without the correct response
#' draw(my_mat, hide = TRUE)
#' # add the correct response
#' draw(correct(my_mat))
#' }
correct.matriks = function(obj) {

    n.cell<-obj$mat.type
  if (n.cell == 9) {
    correct <- obj$Sq9
  } else {
    correct <- obj$Sq4
  }
  return(correct)
}

