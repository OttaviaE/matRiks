#' Difference (method)
#'
#' Difference distractors
#'
#' @param obj the matrikes
#' @param ... other arguments
#' @param seed seed
#'
#' @return An object
#' @export difference
#' @export
#'
#' @examples
difference <- function(obj, seed, ...) {
  UseMethod("difference")
}


#' Difference distractors
#'
#' @param obj the object
#' @param ... Other arguments
#' @param seed the seed
#'
#' @return
#' @export difference.matriks
#' @export
#'
#' @examples
difference.matriks <- function(obj, seed = 666, ...) {
  set.seed(seed)


  hrules <- gsub(".inv", "", obj$hrule)
  vrules <-  gsub(".inv", "", obj$vrule)

  if (any(hrules[order(hrules)] == vrules[order(vrules)]) == TRUE) {
    start_cell <-  obj$Sq3
  } else {
    start_cell <- obj$Sq1
  }

  list_star<-dir(paste0(system.file(package = "matRiks"), "/R"))
  list_star<-list_star[grep("figure",list_star)]
  list_star<-paste0(paste0(system.file(package = "matRiks"), "/R"),list_star)
  list_figures <- multi_list(list_star)
  list_figures <- list_figures[!list_figures$name %in% unlist(obj$Sq1$shape), ]

  if (sum(grepl("d.int", unlist(start_cell))) == 0) {
    over_figure <- sample(list_figures[list_figures$d.int == TRUE, "name"])
    over_figure <- get(over_figure[1])
    if (any(unlist(over_figure()$tag) == "black.figure") == TRUE) {
      over_figure <- size(over_figure(), 3)
      dist_difference <- cof(start_cell,
                             over_figure)
    } else {
      dist_difference <- cof(start_cell,
                             over_figure())
    }
  } else {
    over_figure <- sample(list_figures[list_figures$d.ext == TRUE, "name"])
    over_figure <- get(over_figure[1])
    dist_difference <- cof(start_cell,
                           over_figure())
  }


  class(dist_difference) <- "figure"
  return(dist_difference)


}
