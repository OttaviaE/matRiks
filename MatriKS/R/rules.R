#' Rule for rotating a figure
#'
#' @param obj the object on which the rule is applied
#' @param n A number defining the angle of the rotation. Default is $\pi$.
#' @param rule Define the rotation rules. Default is anticlockwise.`rule = "inv"` forces a clockwise rotation
#' @param ...
#'
#' @return
#' @export
#'
#' @examples one day not todaty

rotate <- function(obj,n,rule,...) {
  UseMethod("rotate")
}

rotate.figure<-function(obj,n=4,rule="rot",...) {
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
reflect <- function(obj,n,...) {
  UseMethod("reflect")
}

reflect.figure<-function(obj,n=2,...) {
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
size <- function(obj,n,rule, ...) {
  UseMethod("size")
}
size.figure<-function(obj,n = 2,  rule = "size", ...) {
  numbers<-unlist(strsplit(rule,split=""))
  num<-2
  for(i in 1:length(numbers))
  {
    if(any(numbers[i]==1:9)){
      num<-which(numbers[i]==1:9)
    }
  }
  if (grepl("inv", rule)) {
    obj$size.x<-Map('+', obj$size.x,(n*.9))
    obj$size.y<-Map('+', obj$size.y,(n*.9))
    obj$pos.x<-Map('+', obj$pos.x,(n*.9))
    obj$pos.y<-Map('-', obj$pos.y,(n*.9))
  } else {
    obj$size.x<-Map('/', obj$size.x,(n*.9))
    obj$size.y<-Map('/', obj$size.y,(n*.9))
    obj$pos.x<-Map('/', obj$pos.x,(n*.9))
    obj$pos.y<-Map('/', obj$pos.y,(n*.9))
  }

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
shape.figure<-function(obj,n = 1,rule = "default",...) {
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

#' Apply logic rules to a figure
#'
#' @param obj
#' @param n
#' @param rule
#' @param seed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
logic <- function(obj,n,rule,seed,...) {
  UseMethod("logic")
}
logic.figure<-function(obj,n = 1,rule = "logic",seed = 1,...) {
  if(length(obj$shape) < 3)
  {
    stop("You must have three forms to apply a logical AND !")
  }
  ##gestione di più immagini
  domain<-1:length(obj$shape)
  obj$visible[domain]<-1
  set.seed(seed)
  fixed<-sample(domain,round(length(obj$shape)/5))
  domain<-setdiff(domain,fixed)
  half<-length(domain)%/%2
  index<-list()
  index[[1]]<-sample(domain,half)
  index[[2]]<-sample(setdiff(domain,index[[1]]),half)

  if(rule=="AND"){
    index[[3]]<-union(index[[1]],index[[2]])
    obj$visible[index[[n]]]<-0
  }else if(rule=="OR"){
    if(n<3){
      obj$visible[index[[n]]]<-0
    }
  }else if(rule=="XOR"){

    index[[3]]<-union(setdiff(domain,union(index[[1]],index[[2]])),fixed)
    obj$visible[index[[n]]]<-0
  }
  return(obj)
}


#' Indentity rule
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
identity <- function(obj,...) {
  UseMethod("identity")
}
identity.figure <- function(obj,...) {
  return(obj)
}

#' Rule for changing the filling of a figure
#'
#' @param obj
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fill <- function(obj,n,...) {
  UseMethod("fill")
}
fill.figure<-function(obj,n = 1,rule = "fill",...){
  if(grepl("par",rule))
  {
    index<-c("line1h","line2h","line12h","line1","line2","line12",
             "line1inv","line2inv","line12inv")
  }else if(grepl("line",rule)){
    index <- rep(c("line12","line12h","line12inv"),3)
  }else{
    index <- rep(c("white","grey","black"),3)
  }

  if(grepl("multi",rule))
  {
    set.seed(n)
    new<-Map("c",obj$shade,sample(1:length(obj$shape),length(obj$shape)))
    obj$shade <-lapply(new, function(x,i,n)
    {
      pos <- index==x[1]

      if(is.na(sum(pos)))
      {
        return(index[n+as.numeric(x[2])])
      }else{
        pos <- which(pos)
        return(index[pos+n+as.numeric(x[2])])
      }
    },i=index,n=n)

  }else{
    obj$shade <- lapply(obj$shade, function(x,i,n)
    {
      pos <- index==x
      if(is.na(sum(pos)))
      {
        return(index[n])
      }else{
        pos <- which(pos)
        return(index[pos[1]+n])
      }
    },index,n)
  }

  return(obj)
}
