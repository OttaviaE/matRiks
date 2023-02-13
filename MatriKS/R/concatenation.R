concatenation <- function(...) {
  UseMethod("concatenation")
}

concatenation.list <- function(...) {
  obj <- Map("c", ...)
  return(obj)
}


concatenation.double <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "double"
  obj
}

concatenation.character <- function(...) {
  obj <- c(...)
  return(obj)
}

concatenation.integer <- function(...) {
  obj <- c(...)
  return(obj)
}
