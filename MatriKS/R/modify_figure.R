#' Functions to modify the visible objects in a figure
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to hide/show/replace
#' @param replacement The object with which an element should be replaced
#'
#' @return
#' @export
#'
#' @examples Arrivano
hide<- function(obj,index) {
  UseMethod("hide")
}
hide.figure<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-integer(length(index))
  return(obj)
}

show<- function(obj,index) {
  UseMethod("show")
}

show.figure<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-rep(1,length(index))
  return(obj)
}
replace <- function(obj,index,replacement) {
  UseMethod("replace")
}

replace.figure<-function(obj,index,replacement)
{
  for(i in 1:length(obj))
  {
    obj[[i]][[index]]<-replacement[[i]][[1]]
  }
  return(obj)
}
