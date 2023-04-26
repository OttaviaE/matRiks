decof<- function(obj) {
  UseMethod("decof")
}

#' Split the elements of a figure
#'
#' Return the elements composing a figure
#'
#' @param The object to be splitted
#'
#' @return
#' @export A list of objects
#'
#' @examples
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
      visible = obj$visible[i],
      tag = obj$tag[i]
    )
    attr(ele, "class") <- "figure"
    newobj[[i]]<-ele
  }
  return(newobj)
}
