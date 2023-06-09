#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
shapes_list<-function(filename)
{

  source(filename,local=TRUE)
  rm(filename)
  name <- ls()

  table<-data.frame(
    name=name,
    num_shapes=integer(length(name)),
    small=logical(length(name)),
    fill= logical(length(name)),
    rotate= logical(length(name))
  )
  for(r in 1:length(name))
  {
    f<-get(name[r])
    obj<-f()
    tags<-unlist(obj$tag)

    if(any(tolower(tags)=="simple"))
    {
      table$num_shapes[r] <- 1
    }else if(any(tolower(tags)=="compose2"))
    {
      table$num_shapes[r] <- 2
    }else if(any(tolower(tags)=="compose4"))
    {
      table$num_shapes[r] <- 4
    }
    table$fill[r]<-any(tolower(tags)=="fill")
    table$small[r]<-any(tolower(tags)=="small")
    table$rotate[r]<-any(tolower(tags)=="rotate")
  }
  return(table)
}
