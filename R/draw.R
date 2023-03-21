#' Draw figures
#'
#' @param obj The object to be draw. Can be a single figure, a matrix, or the responses
#' @param main Print a title? Default is FALSE
#' @param canvas Do you want to overimpose the objects? Default is FALSE
#' @param hide Do you want to hide the figure of the correct response? Default is FALSE
#' @param n.figure How main figure should the matrix have? Default is 9
#' @param bg Choose the color of  the background. Deafult is white
#' @param mar Change margins
#' @param xlim Change the length of the x axis
#'
#' @return A graphic
#' @export
#'
#' @examplespoi
draw <- function(obj, main = NULL, canvas = TRUE,
                 hide = FALSE, n.figure = 9,
                 bg = "white",mar=c(1,1,1,1),xlim=16) {
  UseMethod("draw")
}

# bisogna portare la funzione line dall'altra parte ----

draw.figure<- function(obj, main = NULL, canvas = TRUE,
                     bg = "white",mar=c(1,1,1,1),xlim=16) {
  if (canvas == TRUE)
  {
    DescTools::Canvas(xlim=xlim,mar=mar, main = main, bg = bg)
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
          line(elements[[plotting_lines[[ll]]]],
               obj$shade[[j]][1])

        }
        obj$shade[[j]][1] <- NA
      }
      if(obj$num[[j]][1]==1){

        DescTools::DrawRegPolygon(x = obj$pos.x[[j]], y = obj$pos.y[[j]], rot = obj$rotation[[j]],
                       radius.x = obj$size.x[[j]], radius.y = obj$size.y[[j]], nv = obj$nv[[j]],
                       lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])


      }else{

        DescTools::DrawCircle(x = obj$pos.x[[j]], y = obj$pos.y[[j]],
                   r.out = obj$size.x[[j]],r.in= obj$size.y[[j]],
                   theta.1=obj$theta.1[[j]],
                   theta.2=obj$theta.2[[j]], nv = obj$nv[[j]],
                   lty=obj$lty[[j]],lwd=obj$lwd[[j]],col = obj$shade[[j]])

      }
    }
  }
}
