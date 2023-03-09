#' Title
#'
#' @param obj
#' @param n
#' @param rule
#' @param ...
#'
#' @return
#' @export
#'
#' @examples onje day not todaty

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
