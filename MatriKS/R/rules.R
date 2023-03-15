#' Rule for rotating a figure
#'
#' @param obj the object on which the rule is applied
#' @param n A number defining the angle of the rotation DOBBIAMO PARLARE
#' @param rule in che senso?
#' @param ...
#'
#' @return
#' @export
#'
#' @examples one day not todaty

rotation <- function(obj,n,rule,...) {
  UseMethod("rotation")
}

rotation.cell<-function(obj,n,rule="rot",...) {
  numbers<-unlist(strsplit(rule,split=""))
  num<-4
  for(i in 1:length(numbers))
  {
    if(any(numbers[i]==1:9)){
      num<-which(numbers[i]==1:9)
    }
  }
  if(grepl("inv",rule)){
    obj$rotation<-Map('+', obj$rotation,(n-1)*-pi/num)
    obj$theta.1<-Map('+', obj$theta.1,(n-1)*-pi/num)
    obj$theta.2<-Map('+', obj$theta.2,(n-1)*-pi/num)
  }else{
    obj$rotation<-Map('+', obj$rotation,(n-1)*pi/num)
    obj$theta.1<-Map('+', obj$theta.1,(n-1)*pi/num)
    obj$theta.2<-Map('+', obj$theta.2,(n-1)*pi/num)
  }
  return(obj)
}


#' Rule for reflecting a
#'
#' @param obj the object on which the rule is applied
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
reflection <- function(obj,n,...) {
  UseMethod("reflection")
}

reflection.cell<-function(obj,n,...) {
  obj$rotation<-Map('+', obj$rotation,(n-1)*pi)
  obj$theta.1<-Map('+', obj$theta.1,(n-1)*pi)
  obj$theta.2<-Map('+', obj$theta.2,(n-1)*pi)
  return(obj)
}

#' Rule for resizing an object
#'
#' @param obj
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
size <- function(obj,n,...) {
  UseMethod("size")
}
size.cell<-function(obj,n,...) {
  obj$size.x<-Map('/', obj$size.x,(n*.9))
  obj$size.y<-Map('/', obj$size.y,(n*.9))
  obj$pos.x<-Map('/', obj$pos.x,(n*.9))
  obj$pos.y<-Map('/', obj$pos.y,(n*.9))
  return(obj)
}

#' Change shapes
#'
#' @param obj
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
shape <- function(obj,n,...) {
  UseMethod("shape")
}
shape.cell<-function(obj,n,rule,...) {
  if(length(obj$visible)!=3)
  {
    stop("You must have at least three forms to change shapes!")
  }
  if(grepl("inv",rule))
  {
    index<-c(3:1,3:1,3:1) #TL-LR
  }else{
    index<-c(1:3,1:3,1:3) #TR-LL
  }

  pos<-which(obj$visible==1)
  if(length(pos)>1){
    obj$visible[pos]<-0
    obj$visible[index[n]]<-1
  }else {
    obj$visible[pos]<-0
    obj$visible[index[pos+n]]<-1
  }

  return(obj)
}

