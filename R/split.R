split_mat<- function(obj,vis = TRUE, cell = NULL) {
  UseMethod("split_mat")
}
#' Isolate the objects in a cell
#'
#' @param obj The figure
#' @param cell the index of the cell to be splitted
#' @param vis boh Ah si forse era quando volevo integrare con decof
#'
#' @return
#' @export
#'
#' @examples
#'
split_mat.figure = function(obj, vis = TRUE, cell = NULL) {
  if(vis == TRUE) {
    index_elements<-which(obj$visible==1 & unlist(lapply(obj$num, function(x,y) all(x==y), 1)))
  } else {
    index_elements = 1:length(obj$shape)
  }

  split.m <- vector("list", length(index_elements))
  for (i in 1:length(split.m)) {
    split.m[[i]] <- vector("list", length(obj))
    for (j in 1:length(split.m[[i]])) {
      names(split.m)[i] = obj$shape[index_elements[i]]
      attr(split.m[[i]], "class") = "figure"
      split.m[[i]][[j]] = obj[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(obj)[j]
    }
  }
  return(split.m)
}
#' Isolate the visible objects in a cell
#'
#' @param m The matrix
#' @param cell the index of the cell to be splitted
#' @param vis boh Ah si forse era quando volevo integrare con decof
#'
#' @return
#' @export
#'
#' @examples
#'
split_mat.matrix = function(obj, vis = TRUE, cell = NULL) {
  if (is.null(cell) == T) {
    cell.start = correct(obj, mat.type = mat.type)
  } else {
    cell = paste0("Sq", cell)
    cell.start = obj[[cell]]
  }
  split.m<-split_mat(cell.start, vis = vis)
  return(split.m)
}
correct<- function(obj) {
  UseMethod("correct")
}
#' Extract the cell of the correct response
#'
#' @param obj
#'
#' @return The cell with the correct response, either Sq9 (9-cell matrix) or Sq5 (4-cell matrix)
#' @export
#'
#' @examples
correct.matriKS = function(obj) {
  if (obj$mat.type == 9) {
    correct = m$Sq9
  } else {
    correct = m$Sq4
  }
  return(correct)
}

