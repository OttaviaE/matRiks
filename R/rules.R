#' Rotation rule
#'
#' Apply a rotation of pi/4 to a figure.
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the angle of the rotation. Default is 4, which corresponds to a rotation of pi/4.
#' @param rule Define the rotation rule. Default is counterclockwise.`rule = "inv"` forces a clockwise rotation
#' @param ... Other arguments
#'
#' @return A figure with different rotation coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' # default luck
#' draw(luck())
#'
#' # apply the default rotation on the default luck
#' draw(rotate(luck()))
#'
#' # force clockwise rotation
#' draw(rotate(luck(), rule = "inv"))
#' }

rotate <- function(fig ,n, rule,...) {
  UseMethod("rotate")
}

#' @export rotate.figure
#' @export
rotate.figure<-function(fig,n=4,rule="rotation",...) {
  numbers<-unlist(strsplit(rule,split=""))
  num<-4
  for(i in 1:length(numbers))
  {
    if(any(numbers[i]==1:9)){
      num<-which(numbers[i]==1:9)
    }
  }
  if(grepl("inv",rule)){
    fig$rotation<-Map('+', fig$rotation,(n-1)*-pi/num)
    fig$theta.1<-Map('+', fig$theta.1,(n-1)*-pi/num)
    fig$theta.2<-Map('+', fig$theta.2,(n-1)*-pi/num)
  }else{
    fig$rotation<-Map('+', fig$rotation,(n-1)*pi/num)
    fig$theta.1<-Map('+', fig$theta.1,(n-1)*pi/num)
    fig$theta.2<-Map('+', fig$theta.2,(n-1)*pi/num)
  }
  return(fig)
}


#' Reflection rule
#'
#' Apply a reflection of pi on a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the reflection. Default is 2 which corresponds to a 180 degree (pi) reflection
#' @param ...
#'
#' @return A figure with different rotation coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' # default pacman
#' draw(pacman())
#'
#' # apply the default rotation on the default luck
#' draw(reflect(pacman()))
#' }
reflect <- function(fig,n,...) {
  UseMethod("reflect")
}

#' @export reflect.figure
#' @export
reflect.figure<-function(fig,n=2,...) {
  fig$rotation<-Map('+', fig$rotation,(n-1)*pi)
  fig$theta.1<-Map('+', fig$theta.1,(n-1)*pi)
  fig$theta.2<-Map('+', fig$theta.2,(n-1)*pi)
  return(fig)
}

#' Sizing rule
#'
#' Resize a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the dimension of the sizing. Default is 2.
#' @param rule Define the sizing rule. Default is to reduce the dimension. rule = "inv" forces to increase the dimension.
#' @param ...
#'
#' @return A figure with different size.x (and size.y) coordinates
#' @export size.figure
#' @export
#'
#' @examples
#' \dontrun{
#' # default square
#' draw(square())
#'
#' # apply the default resizing to the default square
#' draw(size(square()))
#'
#' # make the square bigger
#' draw(size(square(), rule = "inv"))
#' }
size <- function(fig,n,rule, ...) {
  UseMethod("size")
}
#' @export size.figure
#' @export
size.figure<-function(fig,n = 2,  rule = "size", ...) {
  numbers<-unlist(strsplit(rule,split=""))
  num<-2
  for(i in 1:length(numbers))
  {
    if(any(numbers[i]==1:9)){
      num<-which(numbers[i]==1:9)
    }
  }
  if (grepl("inv", rule)) {
    fig$size.x<-Map('*', fig$size.x,(n*.6))
    fig$size.y<-Map('*', fig$size.y,(n*.6))
    fig$pos.x<-Map('/', fig$pos.x,(n*.6))
    fig$pos.y<-Map('/', fig$pos.y,(n*.6))
  } else {
    fig$size.x<-Map('/', fig$size.x,(n*.9))
    fig$size.y<-Map('/', fig$size.y,(n*.9))
    fig$pos.x<-Map('/', fig$pos.x,(n*.9))
    fig$pos.y<-Map('/', fig$pos.y,(n*.9))
  }

  return(fig)
}

#' Change the visibility of the shapes in a figure
#'
#' @param fig A vector of figures obtained with the concatenation of figures function (cof()). Three figures are needed.
#' @param n The number of the figure you want to see. Default is 1 (the first figure in cof() is shown). To see the other figures, change n to the position of the figure you want to show.
#' @param Define the non so bene come dirlo, aiuto
#' @param ...
#'
#' @return A list of three figures, only the first of which is visible
#' @export
#'
#' @examples
#' \dontrun{
#' # Three figures, only the first is shown
#' draw(shape(cof(s.lily(), square(), s.star())))
#'
#' # Show the third figure (star)
#' draw(shape(cof(s.lily(), square(), s.star()), n = 3))
#'
#' # Show the first and the second figures
#'  draw(shape(cof(s.lily(), square(), s.star()), n = c(1,2)))
#' }
shape <- function(fig,n,...) {
  UseMethod("shape")
}
#' @export shape.figure
#' @export
shape.figure<-function(fig,n = 1,rule = "default",...) {
  if(length(fig$visible)!=3)
  {
    stop("You must have at least three forms to change shapes! If you already specified three figures, make sure you used the s. ones ;)")
  }
  if(grepl("inv",rule))
  {
    index<-c(3:1,3:1,3:1) #TL-LR
  }else{
    index<-c(1:3,1:3,1:3) #TR-LL
  }

  pos<-which(fig$visible==1)
  if(length(pos)>1){
    fig$visible[pos]<-0
    fig$visible[index[n]]<-1
  }else {
    fig$visible[pos]<-0
    fig$visible[index[pos+n]]<-1
  }

  return(fig)
}

#' Shading of a figure
#'
#' Change the shading of a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the color of the shading Default is 1 (white). Other options are 2 (grey) and 3 (black)
#' @param rule The rule for shading the figure
#' @param ...
#'
#' @return A figure with different shading characteristics
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # draw defaul triangle
#' draw(triangle())
#'
#' # make it grey
#' draw(shade(triangle(), 2))
#'
#' }
shade <- function(fig,n,...) {
  UseMethod("shade")
}
#' @export shade.figure
#' @export
shade.figure<-function(fig,n = 1,rule = "shade",...){
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
    new<-Map("c",fig$shade,sample(1:length(fig$shape),length(fig$shape)))
    fig$shade <-lapply(new, function(x,i,n)
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
    fig$shade <- lapply(fig$shade, function(x,i,n)
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

  return(fig)
}

#' Apply logic rules to different figures
#'
#' @param fig Vector of figures obtained with the concatenation of figures function (`cof()`). Three figures are needed.
#' @param n ???
#' @param rule Define the logic rule to be applied, either `AND`, `OR`, `XOR`
#' @param seed Set the random seed so that the permutations are consistent
#' @param ...
#'
#' @return An object composed of figures combined according to different logic rules
#' @export
#'
#' @examples
logic <- function(fig,n,rule,seed,...) {
  UseMethod("logic")
}
#' @export logic.figure
#' @export
logic.figure<-function(fig,n = 1,rule = "logic",seed = 1,...) {
  if(length(fig$shape) < 3)
  {
    stop("You must have three forms to apply a logical AND !")
  }
  ##gestione di più immagini
  domain<-1:length(fig$shape)
  fig$visible[domain]<-1
  set.seed(seed)
  fixed<-sample(domain,round(length(fig$shape)/5))
  domain<-setdiff(domain,fixed)
  half<-length(domain)%/%2
  index<-list()
  index[[1]]<-sample(domain,half)
  index[[2]]<-sample(setdiff(domain,index[[1]]),half)

  if(rule=="AND"){
    index[[3]]<-union(index[[1]],index[[2]])
    fig$visible[index[[n]]]<-0
  }else if(rule=="OR"){
    if(n<3){
      fig$visible[index[[n]]]<-0
    }
  }else if(rule=="XOR"){

    index[[3]]<-union(setdiff(domain,union(index[[1]],index[[2]])),fixed)
    fig$visible[index[[n]]]<-0
  }
  return(fig)
}


#' Identity rule
#'
#' Apply an identity rule (i.e., no changes)
#'
#' @param fig The figures on which the rules is applied.
#' @param ...
#'
#' @return An object composed of figures combined according to an identity rule
#' @export
#'
#' @examples
identity <- function(fig,...) {
  UseMethod("identity")
}
#' @export identity.figure
#' @export
identity.figure <- function(fig,...) {
  return(fig)
}


