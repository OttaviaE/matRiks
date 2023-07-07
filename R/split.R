#' Split the correct response (Method)
#'
#' Split all the figures in the correct response
#'
#' @param obj The figure
#' @param cell the index of the cell to be split. Default is the correct response
#' @param vis Does the selection involve only the figures visible in the selected cell? Default is TRUE
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures used in the matrix (vis = FALSE)
#'
#' @export split_mat
#' @export
#'
#' @examples
#' \dontrun{
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1)
#' }
split_mat<- function(obj,vis = TRUE, cell = NULL) {
  UseMethod("split_mat")
}
#' Split the correct response
#'
#' Split all the figures in the correct response
#'
#' @param obj The figure
#' @param cell the index of the cell to be split. Default is the correct response
#' @param vis Does the selection involve only the figures visible in the selected cell? Default is TRUE
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures used in the matrix (vis = FALSE)
#'
#' @export split_mat.figure
#' @export
#'
#' @examples
#' \dontrun{
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1)
#' }
split_mat.figure = function(obj, vis = TRUE, cell = NULL) {
  if(vis == TRUE) {
   # index_elements<-which(obj$visible==1 & unlist(lapply(obj$num, function(x,y) all(x==y), 1)))
    index_elements<-which(obj$visible==1)
  } else {
    index_elements <- 1:length(obj$shape)
  }
 if(length(obj$tag)<max(index_elements))
 {
   #Va completato
   tag_index <- unlist(lapply(obj$tag,function(x){as.integer(gsub("compose", "",  x[grepl("compose", x)]))}))
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
#' Split the correct response (Method)
#'
#' Split all the figures in the correct response
#'
#' @param obj The figure
#' @param cell the index of the cell to be split. Default is the correct response
#' @param vis Does the selection involve only the figures visible in the selected cell? Default is TRUE
#'
#' @return A list of figures of length equal to the number of figures visible in the correct response (vis = TRUE) or to all the figures used in the matrix (vis = FALSE)
#' @export split_mat.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # split the elements in the correct response and assign to an object
#' split_m1 <- split_mat(m1)
#' }
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
#' @param obj The matrix
#' @return The correct response of a matriks
#'
#' @export correct
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
correct<- function(obj) {
  UseMethod("correct")
}
#' Correct response (Method)
#'
#' Isolate the correct response from a matriks
#'
#' @param obj The matrix
#' @return The correct response of a matriks
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

