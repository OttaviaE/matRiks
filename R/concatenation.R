#' Concatenation of figures
#'
#' @param ... Vector of figures to be concatened together.
#' @param name Name of the newely created object (See Details).
#' @param single Should the objects be collapsed into a single object? Deafult is FALSE
#'
#' @return An object of class figure
#' @export cof
#' @export
#'
#' @examples
cof <- function(...,name, single) {
  UseMethod("cof")
}
#' Cof Figure
#'
#' @param ... The figures to be concatenated
#' @param name character, the name to assign to the newly created figure. Default is NULL (no name).
#' @param single logical, define whether the newly created figure is considered as single element to e used in shape() or not. Default is FALSE.
#'
#' @export cof.figure
#' @export
cof.figure <- function( ...,name=NULL, single=FALSE) {
  if(single==TRUE)
  {
    obj <- Map("concatenation", ...)
    obj$shape<-name
    obj$visible<-1
  }else{
    obj <- Map("c", ...)
  }
  attr(obj, "class") <- "figure"
  obj
}


#' Cof character
#'
#' Per generalizzare la funzione per concatenzione di matrici ho definito metodo cof anche per i caratteri
#'
#' @param ... Arguments of the function
#'
#' @return
#' @export cof.character
#' @export
#'
#' @examples
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}

#' Concatenation of matrices
#'
#' Hierarchically concatenates 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.
#'
#' @param ... the matrices to be concatenated
#'
#' @return Return a matrix composed of 2+ matrices
#'
#' @export com
#' @export
com <- function(...) {
  UseMethod("com")
}
#' Com of matrices
#'
#' @param ... The matrices to be concatenated
#'
#' @export com.matriks
#' @export
com.matriks <- function(...) {
  obj <- Map("cof", ...)
  attr(obj, "class") <- "matriKS"
  obj
}


#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation
#' @export
concatenation <- function(...) {
  UseMethod("concatenation")
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.list
#' @export
concatenation.list <- function(...) {
  obj <- Map("c", ...)
  return(obj)
}


#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.double
#' @export
concatenation.double <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "double"
  obj
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.character
#' @export
concatenation.character <- function(...) {
  obj <- c(...)
  return(obj)
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.integer
#' @export
concatenation.integer <- function(...) {
  obj <- c(...)
  return(obj)
}
