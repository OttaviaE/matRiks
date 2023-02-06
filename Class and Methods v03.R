# DEFINITION OF THE FOUNDAMENTAL CLASS FOR THE PACKAGE ----
# Written by Andrea Brancaccio and Ottavia Epifania, University of Padua, Italy
# e-mail: andrea.brancaccio@unipd.it
#
##  Class cell ----
cell <- list(
  shape = NULL,
  size.x = list(),
  size.y = list(),
  rotation = list(),
  pos.x = list(),
  pos.y = list(),
  lty =list(),
  lwd = list(),
  type = list(), 
  nv = list(),
  shade =list(),  
  visible = NULL, 
  tag = list()
)
class(cell) <- "cell"

##  Class Matriks ----
Matriks <- list(
  Sq1 = list(),
  Sq2 = list(),
  Sq3 = list(),
  
  Sq4 = list(),
  Sq5 = list(),
  Sq6 = list(),
  
  Sq7 = list(),
  Sq8 = list(),
  Sq9 = list(),
  hrule = list(),
  vrule = list()
)
class(Matriks) <- "Matriks"

##  Class responses ----
responses<-list(
  correct = list(), 
  r.top = list(), 
  r.diag = list(), 
  r.left = list(), 
  wp.copy = list(), 
  wp.matrix = list(), 
  d.union = list(), 
  ic.scale = list(), 
  ic.flip = list(), 
  ic.inc = list(), 
  ic.neg = list(), 
)
class(responses) <- "responses"


#   CELL'S BASIC METHODS ----

## concatenation methods ----
#This concatenation methods should be hide in the code
#they are used solely for the definiton of cof

#' Concatenation of list or vector
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
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

## CONCATENATION OF CELLS ----
#We need to find a different name for it :(

#' Concatenation of field
#'
#' Function for the concatenation of difference fields
#' @return Return a field object
#' @examples
#'
#' @export
cof <- function(...,name, single) {
  UseMethod("cof")
}

cof.cell <- function( ...,name=NULL, single=FALSE) {
  if(single==TRUE)
  {
    obj <- Map("concatenation", ...)
    obj$shape<-name
    obj$visible<-1
  }else{
    obj <- Map("c", ...)  
  }
  attr(obj, "class") <- "cell"
  obj
}

#Per generalizzare la funzione per concatenzione di matrici ho definito 
#il metodo cof anche per i caratteri
cof.character <- function(...) {
  obj <- c(...)
  attr(obj, "class") <- "character"
  obj
}


replace <- function(obj,index,obj2) {
  UseMethod("replace")
}

replace.cell<-function(obj,index,obj2)
{
  for(i in 1:length(obj))
  {
    obj[[i]][[index]]<-obj2[[i]][[1]]
  }
  return(obj)
}

hide<- function(obj,index) {
  UseMethod("hide")
}

hide.cell<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-integer(length(index))
  return(obj)
}

show<- function(obj,index) {
  UseMethod("show")
}

show.cell<-function(obj,index="Full")
{
  if(any(index=="Full"))
  {
    index<-1:length(obj$shape)
  }
  obj$visible[index]<-rep(1,length(index))
  return(obj)
}


decof<- function(obj) {
  UseMethod("decof")
}

decof.cell<-function(obj)
{
  if(length(obj$shape)==1){
    nobj<-length(obj$nv[[1]])
  }else{
    nobj<-length(obj$nv)
  }
  
  if(nobj!=length(obj$shape)){
    name<-rep("token",nobj)
  }else{
    name<-obj$shape
  }
  newobj<-list()
  for(i in 1:nobj)
  {
    ele<-list(
      shape = name[i],
      size.x = obj$size.x[i],
      size.y = obj$size.y[i],
      theta.1 =obj$theta.1[i],
      theta.2 =obj$theta.2[i],
      rotation = obj$rotation[i],
      pos.x = obj$pos.x[i],
      pos.y = obj$pos.y[i],
      lty =obj$lty[i],
      lwd = obj$lwd[i],
      num = obj$num[i],
      nv = obj$nv[i],
      shade =obj$shade[i],  
      visible = obj$visible[i], 
      tag = obj$tag[i]
    )
    attr(ele, "class") <- "field"
    newobj[[i]]<-ele
  }
  return(newobj)
}





split.mat = function(m, cell = NULL, vis = NULL, mat.type = 9) {
  if (is.null(cell) == T) {
    m.start = correct(m, mat.type = mat.type)
  } else {
    cell = paste0("Sq", cell)
    m.start = m[[cell]]
  }
  
  if(is.null(vis) == T) {
    index_elements<-which(m.start$visible==1 & unlist(lapply(m.start$num, all, 1))) 
  } else {
    index_elements = 1:length(m.start$shape)
  }
  
  split.m <- vector("list", length(index_elements))
  for (i in 1:length(split.m)) {
    split.m[[i]] <- vector("list", length(m.start))
    for (j in 1:length(split.m[[i]])) {
      names(split.m)[i] = m.start$shape[index_elements[i]]
      attr(split.m[[i]], "class") = "field"
      split.m[[i]][[j]] = m.start[[j]][index_elements[i]]
      names(split.m[[i]])[j] = names(m.start)[j]
      split.m[[i]][j]$visible = 1
    }
  }
  return(split.m)
}

## DRAW FOR THE CELL CLASS ---- 
draw <- function(obj, main = NULL, canvas = TRUE, 
                 hide = FALSE, n.cell = 9, bg = "white",mar=c(1,1,1,1),xlim=16) {
  UseMethod("draw")
}


draw.cell<- function(obj, main = NULL, canvas = TRUE, bg = "white",mar=c(1,1,1,1),xlim=16) {
  library(DescTools)
  if (canvas == TRUE)
  {
    Canvas(xlim=xlim,mar=mar, main = main, bg = bg)
  }
  for(j in 1:length(obj$shape))
  {
    if(obj$visible[[j]]==1)
    {
      if(grepl("line",obj$shade[[j]][1]))
      {
        elements<-decof(obj)
        plotting_lines<-which(obj$visible==1 & grepl("line",unlist(obj$shade)))
        for(ll in 1:length(plotting_lines)){
          line(elements[[plotting_lines[[ll]]]],obj$shade[[j]][1]) #Pejo tacon che sbrego
          
        }
        obj$shade[[j]][1] <- NA
      }
      if(obj$num[[j]][1]==1){
        
        DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]],
                       radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                       lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
        
        
      }else{
        
        DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]],
                   r.out = obj$size.x[[j]],r.in= obj$size.y[[j]], theta.1=obj$theta.1[[j]],
                   theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
                   lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])
        
      }
    }
  }
}

