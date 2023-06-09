#' Draw
#'
#' Draws single figures or a matrix with 9 or 4 cells
#'
#' @param obj The figure to be drawn. Can be a single figure, a matrix, or the responses
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param ... other arguments
#'
#' @return A graphic
#' @import DescTools
#' @importFrom graphics par
#' @importFrom utils read.csv
#' @export draw
#' @export
#'
#' @examples
#' \dontrun{
#' # draw a circle
#' draw(circle())
#'
#' # draw a circle inside the other
#' draw(size(circle(), 2), canvas = FALSE)
#' }
draw <- function(obj, main = NULL, canvas = TRUE,
                 hide = FALSE,
                 bg = "white",mar=c(1,1,1,1),xlim=16, ...) {
  UseMethod("draw")
}


#' Draw figure
#'
#' Devo ancora capire bene
#'
#' @param obj The figure to be drawn. Can be a single figure, a matrix, or the responses
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param n.figure integer, define the number of cells of the matrix. Default is 9
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#'
#' @return A graphic
#' @export draw.figure
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # draw a circle
#' draw.figure(circle())
#'
#' # draw a circle inside the other
#' draw.figure(size(circle(), 2), canvas = FALSE)
#' }
draw.figure<- function(obj, main = NULL, canvas = TRUE,
                       hide = FALSE,
                       bg = "white",mar=c(1,1,1,1),xlim=16, ...) {
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

#' Draw MATRIKS
#'
#' Devo ancora capire bene
#'
#' @param obj The figure to be drawn. Can be a single figure, a matrix, or the responses
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param n.figure integer, define the number of cells of the matrix. Default is 9
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#'
#' @return A graphic
#' @export draw.matriks
#' @export
#'
#' @examples
#' \dontrun{
#' # draw a circle
#' draw.matriks(circle())
#'
#' # draw a circle inside the other
#' draw.matriks(size(circle(), 2), canvas = FALSE)
#' }
draw.matriks<- function(obj, main = NULL, canvas = TRUE,
                        hide = FALSE,
                        bg = "white",mar=c(1,1,1,1),xlim=16, ...) {
  # ti prego non arrabbiarti
  # come non fungeva più per ragioni che boh, ho capito che aveva bisogno di nuove funzioni (cof.double e cof.numeric)
  # quando commi delle matrici, mat.type prende classe double ed ha lunghezza del numero di matrici
  # che sono state concatenate (questa può essere una info importante per il futuro)
  # sicuramente tu hai una soluzione più elegante ma ora la solzuzione qui and dirty è:
  # il probklema di questa soluzion eè che è stata estesa a tutto
  if (class(obj$mat.type) == "numeric") {
    n.cell<-obj$mat.type
  } else {
    n.cell <- obj$mat.type[1]
  }

  squares <- paste0("Sq", 1:9)
  if (n.cell == 9) {
    par(
      mfrow = c(3, 3),
      mar = c(0.5, 6, 0.5, 2) + .1,
      mai = c(.1, .1, .1, .1),
      oma = c(4, 4, 0.2, 0.2)
    )

  } else if (n.cell == 4) {
    par(
      mfrow = c(2, 2),
      mar = c(0.5, 6, 0.5, 2) + .1,
      mai = c(.1, .1, .1, .1),
      oma = c(4, 4, 0.2, 0.2)
    )
  }

  if (hide == TRUE){n.cell<-n.cell-1}
  for (i in 1:n.cell)
  {
    DescTools::Canvas(xlim=16,mar=c(1,1,1,1), main = main, bg = bg)
    draw(obj[[squares[[i]]]],canvas = FALSE)
  }

}


#' Draw responses
#'
#' Draw the response list
#'
#' @param obj The figure to be drawn. Can be a single figure, a matrix, or the responses
#' @param main logical, print the title of the drawing. Default is FALSE
#' @param canvas logical, draw the figure on a new canvas. Default is TRUE
#' @param hide logical, hide the cell corresponding to the correct response. Default is FALSE
#' @param n.figure integer, define the number of cells of the matrix. Default is 9
#' @param bg character, define the color background. Default is white
#' @param mar numeric vector, change margins of the canvas
#' @param xlim numeric, change the length of the x axis
#' @param distractors the distractors
#' @param print the prrint
#' @param ... Other arguments
#'
#' @return a list
#' @export draw.responses
#' @export
#'
#' @examples
draw.responses <- function(obj, main = NULL, canvas = TRUE,
                           hide = FALSE,
                           bg = "white",mar=c(1,1,1,1),xlim=16,
                           distractors = NULL, print = FALSE,
                           ...) {
  if (is.null(distractors) == TRUE) {
    distractors <- names(obj)
  }

  if (print == FALSE) {
    par(mfrow =c(2, round(length(distractors)/2 +0.2) ),
        mar = c(0.5, 6, 0.5, 2) + .1,
        mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )

  } else {
    par(mfrow = c(1, 1), mar = c(0.5, 6, 0.5, 2) + .1,
        mai=c(.1,.1,.1,.1),oma=c(4,4,0.2,0.2) )
  }

  if (is.null(main) == FALSE) {
    for (i in 1:length(distractors)) {
      draw(obj[[distractors[i]]], main <- distractors[i])
    }
  } else {
    for (i in 1:length(distractors)) {
      draw(obj[[distractors[i]]])
    }
  }

}
