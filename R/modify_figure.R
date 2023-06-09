#' Hide figures
#'
#' Change the visibility of the single objects composing a figure from 1 to 0
#'
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to hide
#'
#' @return An object with changed visibility
#' @export hide
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the triangle
#' draw(show(my_shapes, 2))
#' }
hide<- function(obj,index) {
  UseMethod("hide")
}

#' Hide figures
#'
#' Change the visibility of the single objects composing a figure from 1 to 0
#'
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to hide
#'
#' @return An object with changed visibility
#' @export hide.figure
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the triangle
#' draw(show(my_shapes, 2))
#' }
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
#' @export show
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the triangle
#' draw(show(my_shapes, 2))
#' }
show<- function(obj,index) {
  UseMethod("show")
}
#' Show figures
#'
#' Change the visibility of the single objects composing a figure from 0 to 1
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to show
#'
#' @return An object with changed visibility
#' @export show.figure
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # show the triangle
#' draw(show(my_shapes, 2))
#' }
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
#' @param visible If true it will replace only the visible figure
#'
#' @return An object with a changed figure
#'
#' @return Return an object with a different figure
#' @export replace
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # replace the square with a gray pacman
#' draw(replace(my_shapes, 1, pacman(shd = "grey")))
#' }
replace <- function(obj,index,replacement,visible) {
  UseMethod("replace")
}
#' Replace objects
#'
#' Replace an object in a figure with another object
#'
#' @param obj The figure of a matrix
#' @param index The index of the element to replace
#' @param replacement The name of the figure with which the original one is replaced
#' @param visible If true it will replace only the visible figure
#'
#' @return An object with a changed figure
#'
#' @return Return an object with a different figure
#' @export replace.figure
#' @export
#'
#' @examples
#' \dontrun{
#' # concanate three figures into an object
#' my_shapes = cof(square(), triangle(), slice())
#' # draw object
#' draw(my_shapes)
#' # replace the square with a gray pacman
#' draw(replace(my_shapes, 1, pacman(shd = "grey")))
#' }

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
