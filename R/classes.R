
figure <- list(
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
class(figure) <- "figure"

##  Class matriks ----
matriks <- list(
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
  vrule = list(),
  mat.type=integer()
)
class(matriks) <- "matriks"

##  Class responses ----
responses<-list(
  correct = list(),
  r_top = list(),
  r_diag = list(),
  r_left = list(),
  wp_copy = list(),
  wp_matrix = list(),
  d_union = list(),
  ic_scale = list(),
  ic_flip = list(),
  ic_inc = list(),
  ic_neg = list()
)
class(responses) <- "responses"
