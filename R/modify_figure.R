#' Hide figures
#'
#' Change the visibility of the single objects composing a figure from 1 to 0
#'
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to hide
#'
#' @return An object with changed visibility
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

#' Show figures
#'
#' Change the visibility of the single objects composing a figure from 0 to 1
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to show
#'
#' @return An object with changed visibility
#' @export
#'
#' @examples
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
#' Replace objects
#'
#' Replace an object in a figure with another object
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to replace
#' @param replacement The name of the figure with which the original one is replaced
#'
#' @return An object with a changed figure
#' @param replacement
#'
#' @return
#' @export
#'
#' @examples
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
