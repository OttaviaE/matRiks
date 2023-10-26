#' Concatenation of figures (method)
#'
#' Concatenation of different figures to create a new figure
#'
#' @param ... Vector of figures to be concatenated together
#' @param name character, name of the figure created with cof()
#' @param single logical, force the figure to be a single figure to be used in shape(). Default is FALSE
#'
#' @return An object of class figure
#' @export cof
#' @export
#'
#' @examples
#'
#' # concatenate figures without creating a new figure
#' new_figure <- cof(square(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(square(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
cof <- function(...,name, single) {
  UseMethod("cof")
}
#' @describeIn cof Concatenation of figures (figures)
#'
#' Concatenation of different figures to create a new figure
#'
#' @param ... Vector of figures to be concatenated together
#' @param name character, name of the figure created with cof()
#' @param single logical, force the figure to be a single figure to be used in shape(). Default is FALSE
#'
#' @return An object of class figure
#'
#' @export cof.figure
#' @export
#'
#' @examples
#' # concatenate figures without creating a new figure
#' new_figure <- cof(square(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(square(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
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


#' @describeIn cof Concatenation of character
#'
#' Concatenation of different figures to create a new figure
#'
#' @param ... Vector of figures to be concatenated together
#' @param name character, name of the figure created with cof()
#' @param single logical, force the figure to be a single figure to be used in shape(). Default is FALSE
#' @param ... Arguments of the function
#'
#' @return A concatenation of character
#' @export cof.character
#' @export
#'
#' @examples
#' # concatenate figures without creating a new figure
#' new_figure <- cof(square(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(square(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}

#' @describeIn cof Concatenation of matrices (Method)
#'
#' Hierarchical concatenation of 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.
#'
#' @param ... the matrices to be concatenated
#'
#' @return An object of class matriks resulting from the hierarchical concatenation of the original matrices
#'
#' @export com
#' @export
#'
#' @examples
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
com <- function(...) {
  UseMethod("com")
}
#' @describeIn cof Concatenation of matrices
#'
##' Hierarchical concatenation of 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.
#'
#' @param ... the matrices to be concatenated
#'
#' @return An object of class matriks resulting from the hierarchical concatenation of the original matrices
#'
#' @export com.matriks
#' @export
#' @examples
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
com.matriks <- function(...) {
  obj <- Map("cof", ...)
  attr(obj, "class") <- "matriks"
  obj
}


#' @describeIn cof Concatenation (Method)
#'
#' @param ... The objects to be concanted
#'
#' @export concatenation
#' @export
#'
#' @examples
#' # concatenate two characters
#' concatenation("a", "b")
concatenation <- function(...) {
  UseMethod("concatenation")
}

#' @describeIn cof Concatenation of lists
#'
#' @param ... The lists to be concatenated
#'
#' @export concatenation.list
#' @export
#' @examples
#' # create some lists
#' a <- list(letters[c(14,13)], LETTERS[c(4, 3)])
#' b <- list(letters[c(12, 13)], LETTERS[c(4, 3)])
#' concatenation(a, b)
concatenation.list <- function(...) {
  obj <- Map("c", ...)
  return(obj)
}


#' @describeIn cof Concatenation of double
#'
#' @param ... The objects of class double to be concatenated
#'
#' @export concatenation.double
#' @export
#' @examples
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
concatenation.double <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "double"
  obj
}

#' @describeIn cof Concatenation of double
#'
#' @param ... The objects of class double to be concatenated
#'
#' @export concatenation.double
#' @export
#'
#' @examples
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
cof.double <- function(...) {
  obj <- unique(c(...))
  attr(obj, "class") <- "double"
  obj
}

#' @describeIn cof Concatenation of numeric
#'
#' @param ... The objects of class numeric to be concatenated
#'
#' @export cof.numeric
#' @export
#'
#' @examples
#' # concatenate two numeric
#' cof(rnorm(1, 25), rnorm(4, 34))
cof.numeric <- function(...) {
  obj <- unique(c(...))
  attr(obj, "class") <- "numeric"
  obj
}

#' @describeIn cof Concatenation of characters
#'
#' @param ... The objects of class character to be concatenated
#'
#' @export cof.character
#' @export
#'
#' @examples
#' # concatenate two numeric
#' cof("a", "b", "d")
concatenation.character <- function(...) {
  obj <- c(...)
  return(obj)
}

#' @describeIn cof Concatenation of stuff
#'
#' @param ... The to be concatenated
#'
#' @export concatenation.integer
#' @export
#'
#' @examples
#' # concatenate two numeric
#' cof(1:3, 22:20)
concatenation.integer <- function(...) {
  obj <- c(...)
  return(obj)
}
