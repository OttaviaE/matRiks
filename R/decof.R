#' Split the elements of a figure (Method)
#'
#' Return the elements composing a figure
#'
#' @param obj The figure of class figure to be split in its single components
#'
#' @return A named list of figures of length equal to the total of shapes in a figure (both visible and not visible)
#' @export decof
#' @export
#'
#' @examples
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat1 <- mat_apply(triangle(), hrules = "size")
#' my_mat2 <- mat_apply(dot(), hrules = "shade")
#' my_mat <- com(my_mat1, my_mat2)
#' # Return the figures composing the first cell of the matriks
#' decof(my_mat$Sq2)
decof<- function(obj) {
  if (inherits(obj, "figure") == FALSE) {
    stop("decof can only be applied to objects of class figure")
  }
  UseMethod("decof")
}

#' @describeIn decof Split the elements of a figure
#'
#' Return the elements composing a figure
#'
#' @param obj The figure of class figure to be split in its single components
#'
#' @return A named list of figures of length equal to the total of shapes in a figure (both visible and not visible)
#' @export decof.figure
#' @export
#'
#' @examples
#' # apply the size rule on a triangle for creating a matriks with 9 cell
#' my_mat1 <- mat_apply(triangle(), hrules = "size")
#' my_mat2 <- mat_apply(dot(), hrules = "shade")
#' my_mat <- com(my_mat1, my_mat2)
#' # Return the figures composing the first cell of the matriks
#' decof(my_mat$Sq2)
decof.figure<-function(obj) {

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
