#' Concatenation of objects
#'
#' @param ... Vector of objects to be concatened together.
#' @param name Name of the newely created object (See Details).
#' @param single Should the objects be collapsed into a single object? Deafult is FALSE
#'
#' @return An object of class cell
#' @export
#'
#' @examples Poi li scrivo un attimo
cof <- function(...,name, single) {
  UseMethod("cof")
}

cof.cell <- function( ...,name=NULL, single=FALSE) {
  if(single==TRUE)
  {
    obj <- Map("concatenation", ...)
    obj$shape<-name
    obj$visible<-1
  }else{
    obj <- Map("c", ...)
  }
  attr(obj, "class") <- "cell"
  obj
}

#Per generalizzare la funzione per concatenzione di matrici ho definito
#il metodo cof anche per i caratteri
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}
