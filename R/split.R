split_mat<- function(obj) {
  UseMethod("split_mat")
}
#' Isolate ONLY the visible objects in a cell
#'
#' @param m The matrix
#' @param cell the index of the cell to be splitted
#' @param vis boh Ah si forse era quando volevo integrare con decof
#'
#' @return
#' @export
#'
#' @examples
split_mat.figure = function(m, cell = NULL, vis = NULL) {
  if (is.null(cell) == T) {
    m.start = correct(m, mat.type = mat.type)
  } else {
    cell = paste0("Sq", cell)
    m.start = m[[cell]]
  }

  if(is.null(vis) == T) {
    index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1)))
  } else {
    index_elements = 1:length(m.start$shape)
  }

  split.m <- vector("list", length(index_elements))
  for (i in 1:length(split.m)) {
    split.m[[i]] <- vector("list", length(m.start))
    for (j in 1:length(split.m[[i]])) {
      names(split.m)[i] = m.start$shape[index_elements[i]]
      attr(split.m[[i]], "class") = "figure"
      split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(m.start)[j]
      split.m[[i]][j]$visible = 1
    }
  }
  return(split.m)
}
correct<- function(obj) {
  UseMethod("correct")
}
#' Extract the cell of the correct response
#'
#' @param m
#' @param mat.type
#'
#' @return The cell with the correct response, either Sq9 (9-cell matrix) or Sq5 (4-cell matrix)
#' @export
#'
#' @examples
correct.matriKS = function(m, mat.type = 9) {
  if (mat.type == 9) {
    correct = m$Sq9
  } else {
    correct = m$Sq5
  }

  return(correct)
}
