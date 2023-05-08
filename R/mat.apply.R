mat_apply <- function(Sq1,mat.type=9,hrules = "identity", vrules = "identity") {
  #Definition of the matRiks
  obj <- list()
  squares <- paste0("Sq", 1:9)
  for (i in 1:length(squares))
  {
    if(i<=mat.type)
    {
      obj[[squares[i]]] <- Sq1 #Copy the same field in all the cells
    }else{
      obj[[squares[i]]] <- NULL #Initialized as empty
    }

  }
  obj$hrule <- hrules
  obj$vrule <- vrules
  obj$mat.type <- mat.type

  if(mat.type==4)
  {
    # The rules are applied by row keeping fixed the row by means of the three vectors
    row_1 <- paste0("Sq", 1:2)
    row_2 <- paste0("Sq", 3:4)

    # The rules are applied by column keeping fixed the row by means of the three vectors
    col_1 <- paste0("Sq", c(1,3))
    col_2 <- paste0("Sq", c(2,4))
    nrow<-2
  }else{
    # The rules are applied by row keeping fixed the row by means of the three vectors
    row_1 <- paste0("Sq", 1:3)
    row_2 <- paste0("Sq", 4:6)
    row_3 <- paste0("Sq", 7:9)

    # The rules are applied by column keeping fixed the row by means of the three vectors
    col_1 <- paste0("Sq", seq(1, 9, 3))
    col_2 <- paste0("Sq", seq(2, 9, 3))
    col_3 <- paste0("Sq", seq(3, 9, 3))
    nrow<-3
  }


  #This table contains in the first row the label of the rules
  #and in the second row the function associated
  function_list <- read.csv("R/function_list.prn", sep="")

    for (r in 1:length(hrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,hrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"]
      }
      f<-get(nth_rule )
      for (i in 1:nrow)
      {
        obj[[row_1[i]]] <- f(obj[[row_1[i]]],i,hrules[r],seed=1)
        obj[[row_2[i]]] <- f(obj[[row_2[i]]],i,hrules[r],seed=5)
        if(mat.type==9){obj[[row_3[i]]] <- f(obj[[row_3[i]]],i,hrules[r],seed=6)}
      }
    }

    #applying the vertical rules
    for (r in 1:length(vrules))
    {
      nth_rule<-function_list$function.[unlist(lapply(function_list$label,grepl,vrules[r]))]
      if(length(nth_rule)>1)
      {
        nth_rule <-unique(nth_rule)
        nth_rule <-nth_rule[!nth_rule=="fill"]
      }
      f<-get(nth_rule )
      for (i in 1:nrow)
      {
        obj[[col_1[i]]] <- f(obj[[col_1[i]]],i,vrules[r],seed=1)
        obj[[col_2[i]]] <- f(obj[[col_2[i]]],i,vrules[r],seed=5)
        if(mat.type==9){obj[[col_3[i]]] <- f(obj[[col_3[i]]],i,vrules[r],seed=6)}
      }
    }

  attr(obj, "class") <- "matriks"
  return(obj)
}
