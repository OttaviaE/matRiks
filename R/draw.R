#' Draw figures
#'
#'
#' @param obj The object to be draw. Can be a single figure, a matrix, or the responses
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param n.figure integer, define the number of cells of the matrix. Default is 9
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
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

# eliminiamo il riempimento a righe  ----

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
