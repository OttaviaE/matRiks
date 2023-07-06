#' Split the elements of a figure
#'
#' Return the elements composing a figure
#'
#' @param obj The figure to be split in its single components
#'
#' @return A named list of figures
#' @export decof
#' @export
#'
#' @examples
#' \dontrun{
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat = mat_apply(triangle(), mat.type = 9,
#' hrule = "size")
#' # Return the figures composing the first cell of the matriks
#' decof(my_mat)
#' }
decof<- function(obj) {
  UseMethod("decof")
}

#' Decof
#'
#' @param obj The object to split
#'
#' @export decof.figure
#' @export
decof.figure<-function(obj)
{
  if(length(obj$shape)==1){
    nobj<-length(obj$nv[[1]])
  }else{
    nobj<-length(obj$nv)
  }

  if(nobj!=length(obj$shape)){
    name<-rep("token",nobj)
  }else{
    name<-obj$shape
  }
  newobj<-list()
  for(i in 1:nobj)
  {
    ele<-list(
      shape = name[i],
      size.x = obj$size.x[i],
      size.y = obj$size.y[i],
      theta.1 =obj$theta.1[i],
      theta.2 =obj$theta.2[i],
      rotation = obj$rotation[i],
      pos.x = obj$pos.x[i],
      pos.y = obj$pos.y[i],
      lty =obj$lty[i],
      lwd = obj$lwd[i],
      num = obj$num[i],
      nv = obj$nv[i],
      shade =obj$shade[i],
      visible = obj$visible,
      tag = obj$tag[i]
    )
    attr(ele, "class") <- "figure"
    newobj[[i]]<-ele
  }
  return(newobj)
}
