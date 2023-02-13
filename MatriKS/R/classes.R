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
  ic.neg = list()
)
class(responses) <- "responses"
