#' Hide figures (Method)
#'
#' Change the visibility of a figure from 1 to 0
#'
#' @param obj A figure composed of different figures
#' @param index integer, the index of the element to hide
#'
#' @return The starting object with a hidden figure
#' @export hide
#' @export
#'
#' @examples
#' # concatenate three figures into an object
#' my_shapes <- cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # hide the triangle
#' draw(hide(my_shapes, 2))
hide<- function(obj,index) {
  UseMethod("hide")
}
#' Hide figures
#'
#' Change the visibility of a figure from 1 to 0
#'
#' @param obj A figure composed of different figures
#' @param index integer, the index of the element to hide
#'
#' @return The starting object with a hidden figure
#' @export hide.figure
#' @export
#'
#' @examples
#' # concatenate three figures into an object
#' my_shapes <- cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # hide the triangle
#' draw(hide(my_shapes, 2))
hide.figure<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-integer(length(index))
  return(obj)
}

#' Show figures (Method)
#'
#' Change the visibility of a figure from 0 to 1
#'
#' @param obj A figure composed of different figures
#' @param index integer, the index of the element to hide
#'
#' @return The starting object with one more visible figure
#' @export show
#' @export
#'
#' @examples
#' # concatenate three figures into an object. The first figure is not visible
#' my_shapes <- cof(square(vis = 0), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the square
#' draw(show(my_shapes, 1))
show<- function(obj,index) {
  UseMethod("show")
}
#' @describeIn show Show figures
#'
#' Change the visibility of a figure from 0 to 1
#'
#' @param obj A figure composed of different figures
#' @param index integer, the index of the element to hide
#'
#' @return The starting object with one more visible figure
#' @export show.figure
#' @export
#'
#' @examples
#' # concatenate three figures into an object. The first figure is not visible
#' my_shapes <- cof(square(vis = 0), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the square
#' draw(show(my_shapes, 1))
show.figure<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-rep(1,length(index))
  return(obj)
}
#' Replace figures (Method)
#'
#' Replace a figure with another figure
#'
#' @inheritParams show
#' @param replacement The figure with which the original one is replaced
#' @param visible logical, if TRUE it will replace only the visible figure. Default is FALSE
#'
#' @return An object with a changed figure
#'
#' @return The starting object with a replaced figure
#' @export replace
#' @export
#'
#' @examples
#' # concanate three figures into an object
#' my_shapes <- cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # replace the square with a gray pacman
#' draw(replace(my_shapes, 1, pacman(shd = "grey")))
replace <- function(obj,index,replacement,visible) {
  UseMethod("replace")
}
#' @describeIn replace Replace figures
#'
#' Replace a figure with another figure
#'
#' @inheritParams show
#' @param replacement The figure with which the original one is replaced
#' @param visible logical, if TRUE it will replace only the visible figure. Default is FALSE
#'
#' @return An object with a changed figure
#'
#' @return The starting object with a replaced figure
#' @export replace.figure
#' @export
#'
#' @examples
#' # concanate three figures into an object
#' my_shapes <- cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # replace the square with a gray pacman
#' draw(replace(my_shapes, 1, pacman(shd = "grey")))
replace.figure<-function(obj,index,replacement,visible=FALSE)
{
  if(visible==TRUE)
  {
    vis<-which(obj$visible==1)
    index<-vis[index]
  }

  for(i in 1:length(obj))
  {
    obj[[i]][[index]]<-replacement[[i]][[1]]
  }
  return(obj)
}
