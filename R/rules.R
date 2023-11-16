#' Rotation rule (Method)
#'
#' Apply a rotation of a fixed angle to a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the angle of the rotation. Default is 4, which corresponds to a rotation of \eqn{{4}\alpha}
#' @param rule character, defines the rotation rule. Default is counterclockwise. If the  rule arguments contain the string "inv" forces a clockwise rotation. Each corresponds to an \eqn{\alpha=\frac{1}{k}\pi}. Default \eqn{k} is 4. To change the value of \eqn{k} is sufficient to add a number from 1 to 9 in the argument.
#' @param ... Other arguments
#'
#' @return A figure of class figure with different rotation coordinates
#' @export rotate
#' @export
#'
#' @examples
#' # default luck
#' draw(luck())
#'
#' # apply the default rotation on the default luck
#' draw(rotate(luck()))
#'
#' # force clockwise rotation
#' draw(rotate(luck(), rule = "inv"))
rotate <- function(fig ,n, rule,...) {
  UseMethod("rotate")
}

#' @describeIn rotate Rotate a figure
#'
#' Apply a rotation of a fixed angle to a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the angle of the rotation. Default is 4, which corresponds to a rotation of \eqn{{4}\alpha}
#' @param rule character, defines the rotation rule. Default is counterclockwise. If the  rule arguments contain the string "inv" forces a clockwise rotation. Each corresponds to an \eqn{\alpha=\frac{1}{k}\pi}. Default \eqn{k} is 4. To change the value of \eqn{k} is sufficient to add a number from 1 to 9 in the argument.
#' @param ... Other arguments
#'
#' @return A figure of class figure with different rotation coordinates
#' @export rotate.figure
#' @export
#'
#' @examples
#' # default luck
#' draw(luck())
#'
#' # apply the default rotation on the default luck
#' draw(rotate(luck()))
#'
#' # force clockwise rotation
#' draw(rotate(luck(), rule = "inv"))
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


#' Reflection rule (Method)
#'
#' Apply a rotation of \eqn{\pi} to a figure.
#'
#' @param fig The figure to be reflected
#' @param n integer, defines the angle of the rotation. Default is 2
#' @param ... Other arguments
#'
#' @return A figure of class figure with different rotation coordinates
#' @export reflect
#' @export
#'
#' @examples
#' # default pacman
#' draw(pacman())
#'
#' # apply the default reflection on the default pacman
#' draw(reflect(pacman()))
reflect <- function(fig,n,...) {
  UseMethod("reflect")
}
#' @describeIn reflect Reflect a figure
#'
#' Apply a rotation of \eqn{\pi} to a figure.
#'
#' @param fig The figure to be reflected
#' @param n integer, defines the angle of the rotation. Default is 2
#' @param ... Other arguments
#'
#' @return A figure of class figure with different rotation coordinates
#' @export reflect.figure
#' @export
#'
#' @examples
#' # default pacman
#' draw(pacman())
#'
#' # apply the default reflection on the default pacman
#' draw(reflect(pacman()))
reflect.figure<-function(fig,n=2,...) {
  fig$rotation<-Map('+', fig$rotation,(n-1)*pi)
  fig$theta.1<-Map('+', fig$theta.1,(n-1)*pi)
  fig$theta.2<-Map('+', fig$theta.2,(n-1)*pi)
  return(fig)
}

#' Sizing rule (Method)
#'
#' Apply a resizing to a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the dimension of the sizing. Default is 2
#' @param rule Define the sizing rule. Default is to reduce the dimension.  If the  rule arguments contain the string "inv" size is increased
#' @param ... Other arguments
#'
#' @return A figure of class figure with different size.x and size.y
#' @export size
#' @export
#'
#' @examples
#' # default square
#' draw(square())
#'
#' # apply the default resizing to the default square
#' draw(size(square()))
#'
#' # make the square bigger
#' draw(size(square(), rule = "inv"))
size <- function(fig,n,rule, ...) {
  UseMethod("size")
}
#' @describeIn size Resize a figure
#'
#' @param fig The figure on which the rule is applied
#' @param n A number defining the dimension of the sizing. Default is 2.
#' @param rule Define the sizing rule. Default is to reduce the dimension. rule = "inv" forces to increase the dimension.
#' @param ... Other arguments
#'
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

#' Shape rule (Method)
#'
#' Apply a change in figures rule by change the visibility of the shapes in a figure
#'
#' @param fig A vector of figures obtained with the concatenation of figures function (cof()). Three figures are needed
#' @param n vector, the index of the element to see. Default is 1 (the first figure in cof() is shown). To see the other figures, change n to index the figure you want to show
#' @param rule character, defines the rule for shading the figure
#' @param ... Other arguments
#'
#' @return An object of class figures, only the first figure is visible
#' @export shape
#' @export
#'
#' @examples
#' # Three figures, only the first is shown
#' draw(shape(cof(s_lily(), square(), s_star())))
#'
#' # Show the third figure (star)
#' draw(shape(cof(s_lily(), square(), s_star()), n = 3))
#'
#' # Show the first and the second figures
#'  draw(shape(cof(s_lily(), square(), s_star()), n = c(1,2)))
shape <- function(fig,n, rule, ...) {
  UseMethod("shape")
}
#' @describeIn shape Change the visible shapes
#'
#' @param fig A vector of figures obtained with the concatenation of figures function (cof()). Three figures are needed
#' @param n integer, the index of the element to see. Default is 1 (the first figure in cof() is shown). To see the other figures, change n to index the figure you want to show
#' @param rule character, defines the rule for shading the figure
#' @param ... Other arguments
#'
#' @export shape.figure
#' @export
shape.figure<-function(fig,n = 1,rule = "shape",...) {
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

#' Shade rule (Method)
#'
#' Apply a change in the shading of the figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the color of the shading. Default is 1 (white). Other options are 2 (grey) and 3 (black)
#' @param rule character, defines the rule for shading the figure
#' @param ... Other arguments
#'
#' @return An object of class figure with different shading characteristics
#' @export shade
#' @export
#'
#' @examples
#' # draw default triangle
#' draw(triangle())
#'
#' # make it grey
#' draw(shade(triangle(), 2))
shade <- function(fig, n, rule, ...) {
  UseMethod("shade")
}
#' @describeIn shade Change the shade of a figure
#'
#' Apply a change in the shading of the figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the color of the shading. Default is 1 (white). Other options are 2 (grey) and 3 (black)
#' @param rule character, defines the rule for shading the figure
#' @param ... Other arguments
#'
#' @return An object of class figure with different shading characteristics
#' @export shade.figure
#' @export
#'
#' @examples
#' # draw default triangle
#' draw(triangle())
#'
#' # make it grey
#' draw(shade(triangle(), 2))
#' @export shade.figure
#' @export
shade.figure<-function(fig,n = 1,rule = "shade",...){

    index <- rep(c("white","grey","black"),3)

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
      lx <- length(x)
      pos <- index==x[1]
      if(is.na(sum(pos)))
      {
        return(index[n])
      }else{
        pos <- which(pos)
        return(rep(index[pos[1]+n],lx))
      }
    },index,n)
  }

  return(fig)
}

#' Margin rule (Method)
#'
#' Apply a change in the margins of the figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the linetype of the linewidth
#' @param rule character, lty changes the linetype (1 = solid, 2 = dashed, 3 = dotted), lwd changes the linewidth
#' @param ... Other arguments
#'
#' @return A figure with changed margins
#' @export margin
#' @export
#'
#' @examples
#' # draw default triangle
#' draw(triangle())
#'
#' # change the linetype
#' draw(margin(triangle(), "lty", 2))
margin <- function(fig,n,rule,...) {
  UseMethod("margin")
}


#' @describeIn margin Change the margins rule
#'
#' Apply a change in the margins of the figure
#'
#' @param fig The figure on which the rule is applied
#' @param n integer, defines the linetype of the linewidth
#' @param rule character, lty changes the linetype (1 = solid, 2 = dashed, 3 = dotted), lwd changes the linewdith
#' @param ... Other arguments
#'
#' @return A figure with changed margins
#' @export margin
#' @export
#'
#' @examples
#' # draw default triangle
#' draw(triangle())
#'
#' # change the linetype
#' draw(margin(triangle(),"lty", 2))
margin.figure <-function(fig,n,rule,...){
  if(grepl("inv",rule))
  {
    index<-c(3:1,3:1,3:1) #TL-LR
  }else{
    index<-c(1:3,1:3,1:3) #TR-LL
  }
  # if (rule == "margin") {
  #   obj$lwd<- lapply(obj$lwd,function(x,i,n){i[x+n]+1},index,n)
  #   obj$lty<- lapply(obj$lty,function(x,i,n){i[x+n]},index,n)
  # }  else
  if(grepl("lwd",rule)){
    fig$lwd<- lapply(fig$lwd,function(x,i,n){i[x+n]+1},index,n)
  }else if(grepl("lty",rule)){
    fig$lty<- lapply(fig$lty,function(x,i,n){i[x+n]},index,n)
  }
  return(fig)
}


#' Logical rules (Method)
#'
#' Apply logical rules (intersection--AND, union--OR, symmetrical difference--XOR) to a concatenation of figures
#'
#' @param fig Vector of figures obtained with the concatenation of figures function (`cof()`). Three figures are needed.
#' @param n integer, defines the elements of the logical expression. n=1 and n=2 are the concatenation of figures to which the logical operation is applied. n=3 is the result of the operation.
#' @param rule character, logic rule to be applied, either `AND`, `OR`, `XOR`
#' @param seed integer, Set the random seed so that the permutations are consistent
#' @param ... Other arguments
#' @return An object that is the logical combination of the figures
#' @export logic
#' @export
#'
#' @examples
#' draw(logic(cof(square(), malta(), circle()), "AND"))
#'
logic <- function(fig,n,rule,seed,...) {
  UseMethod("logic")
}
#' @describeIn logic Logical rules
#'
#' Apply logical rules (intersection--AND, union--OR, symmetrical difference--XOR) to a concatenation of figures
#'
#' @param fig Vector of figures obtained with the concatenation of figures function (`cof()`). Three figures are needed.
#' @param n integer, defines the elements of the logical expression. n=1 and n=2 are the concatenations of figures to which the logical operation is applied. n=3 is the result of the operation.
#' @param rule character, logic rule to be applied, either `AND`, `OR`, `XOR`
#' @param seed integer, Set the random seed so that the permutations are consistent
#' @param ... Other arguments
#' @return An object that is the logical combination of the figures
#'
#' @export logic.figure
#' @export
#'
#' @examples
#' draw(logic(cof(square(), malta(), circle()), "AND"))
#'
logic.figure<-function(fig,n = 1,rule = "logic",seed = 1,...) {
  if(length(fig$shape) < 3)
  {
    stop("You must have three figures to apply a logical rule!")
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
#' Identity rule (Method)
#'
#' Apply an identity rule to the figures in a matrix (i.e., no changes)
#'
#' @param fig The figures on which the rules is applied.
#' @param ... Other arguments
#'
#' @return An object composed of figures combined according to an identity rule
#' @export identity
#' @export
#'
#' @examples
#' # generate a matrix with 9 squares
#' draw(mat_apply(square(), hrules = "identity"))
identity <- function(fig,...) {
  UseMethod("identity")
}
#' @describeIn identity Identity figure
#'
#' @param fig Vector of figures obtained with the concatenation of figures function (`cof()`). Three figures are needed.
#' @param ... Other arguments
#'
#' @export identity.figure
#' @export
#'
#' @examples
#' # generate a matrix with 9 squares
#' draw(mat_apply(square(), hrules = "identity"))
identity.figure <- function(fig,...) {
  return(fig)
}


