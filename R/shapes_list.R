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
     fill= logical(length(name)),
     rotate= logical(length(name)),
     d.int= logical(length(name)),
     d.ext= logical(length(name))
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
     table$rotate[r]<-any(tolower(tags)=="rotate")
     table$d.int[r]<-any(tolower(tags)=="d.int")
     table$d.ext[r]<-any(tolower(tags)=="d.ext")
     table$no.d[r]<-any(tolower(tags)=="no.d")
     table$vert[r]<-any(tolower(tags)=="vert")
     table$hor[r]<-any(tolower(tags)=="hor")
     table$inv[r]<-any(tolower(tags)=="inv")
     table$up[r]<-any(tolower(tags)=="up")
     table$down[r]<-any(tolower(tags)=="down")
     table$left[r]<-any(tolower(tags)=="left")
     table$right[r]<-any(tolower(tags)=="right")
   }

  return(table)
}

#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
multi_list<-function(filename)
{
  table<-NULL
  for(i in 1:length(filename))
  {
    table<-rbind(table,shapes_list(filename[i]))
  }
  return(table)
}
