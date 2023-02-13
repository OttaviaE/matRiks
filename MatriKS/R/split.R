#' Isolate ONLY the visible objects in a cell
#'
#' @param m The matrix
#' @param cell the index of the cell to be splitted
#' @param vis boh
#' @param mat.type Does the matrix have 4 or 9 cells? Default is 9
#'
#' @return
#' @export
#'
#' @examples
split.mat = function(m, cell = NULL, vis = NULL, mat.type = 9) {
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
      attr(split.m[[i]], "class") = "field"
      split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(m.start)[j]
      split.m[[i]][j]$visible = 1
    }
  }
  return(split.m)
}
