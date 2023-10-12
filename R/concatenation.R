#' Cof (Method)
#'
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
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
cof <- function(...,name, single) {
  UseMethod("cof")
}
#' Cof (Method)
#'
#' Concatenation of figures
#'
#' @param ... The figures to be concatenated
#' @param name character, the name to assign to the newly created figure. Default is NULL (no name).
#' @param single logical, define whether the newly created figure is considered as single element to e used in shape() or not. Default is FALSE.
#'
#' @return An object of class figure
#'
#' @export cof.figure
#' @export
#' @examples
#' \dontrun{
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
#' }
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


#' Cof character (Method)
#'
#' This allow for concatenating matrices
#'
#' @param ... Arguments of the function
#'
#' @return A concatenation of character
#' @export cof.character
#' @export
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}

#' Concatenation of matrices (Method)
#'
#' Hierarchically concatenates 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.
#'
#' @param ... the matrices to be concatenated
#'
#' @return Return a matrix composed of 2+ matrices
#'
#' @export com
#' @export
#'
#' @examples
#' \dontrun{
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
#'
#' }
com <- function(...) {
  UseMethod("com")
}
#' Concatenation of matrices
#'
#' Hierarchically concatenates 2+ matrices on top of one another. The first matrix is placed on the bottom, the last matrix is placed on top of all other matrices.

#'
#' @param ... The matrices to be concatenated
#'
#' @export com.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
#'
#' }
com.matriks <- function(...) {
  obj <- Map("cof", ...)
  attr(obj, "class") <- "matriks"
  obj
}


#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation
#' @export
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
concatenation <- function(...) {
  UseMethod("concatenation")
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.list
#' @export
#' @examples
#' \dontrun{
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
#'
#' }
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
#' @examples
#' \dontrun{
#' # create the first layer matrix
#' m1 <- mat_apply(hexagon(), hrules = "lty")
#' # create the second matrix
#' m2 <- mat_apply(size(malta(), 2), vrules = "shade")
#' # concatenate the matrices
#' the_mat <- com(m1, m2)
#' # draw the final matrix
#' draw(the_mat)
#'
#' }
concatenation.double <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "double"
  obj
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export cof.double
#' @export
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
cof.double <- function(...) {
  obj <- unique(c(...))
  attr(obj, "class") <- "double"
  obj
}

#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export cof.numeric
#' @export
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
cof.numeric <- function(...) {
  obj <- unique(c(...))
  attr(obj, "class") <- "numeric"
  obj
}



#' Concatenation of stuff
#'
#' @param ... The matrices to be concatenated
#'
#' @export concatenation.character
#' @export
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
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
#'
#' @examples
#' \dontrun{
#' # concatenate figures without creating a new figure
#' new_figure <- cof(suqare(), size(malta(), 2))
#' # structure of new_figure
#' str(new_figure)
#' # concatenate figures and create a new figure
#' my_figure <- cof(suqare(), size(malta(), 2),
#'                   single = TRUE,
#'                    name = "my_figure")
#' # structure of new_figure
#'  str(my_figure)
#' }
concatenation.integer <- function(...) {
  obj <- c(...)
  return(obj)
}
