category.mat <- function(input = "immediate"){
  # -------------------------------------------------------------------------- #
  # define the categoires as matrices
  # c(mixed/immediate, dyn/static, cancelation/not,
  #   piecemeal/not, superimp/not, full/partial,
  #   halves/center, circular/random, motion/not)
  dict_categories <- list("Cancellation",          matrix(c(1,0,1,0,0,1,0,0,0), nrow = 3),
                       "Center/Surround",          matrix(c(1,0,0,1,0,0,0,0,0), nrow = 3),
                       "Circular motion of dots",  matrix(c(1,0,0,0,0,1,0,1,1), nrow = 3),
                       "Circular wave",            matrix(c(1,1,0,1,0,0,1,1,0), nrow = 3),
                       "Dispersing/Gathering",     matrix(c(1,1,0,1,0,0,0,0,0), nrow = 3),
                       "Dynamic piecemeal",        matrix(c(1,1,0,1,0,1,0,0,0), nrow = 3),
                       "Dynamic superimposed",     matrix(c(1,1,0,0,1,1,0,0,0), nrow = 3),
                       "Dynamic superimposed piecemeal",  matrix(c(1,1,0,1,1,0,0,0,0), nrow = 3),
                       "Halves",                   matrix(c(1,0,0,1,0,0,1,0,0), nrow = 3),
                       "Immediate",                matrix(c(0,0,0,0,0,0,0,0,0), nrow = 3),
                       "Partial cancellation",     matrix(c(1,0,1,0,0,0,0,0,0), nrow = 3),
                       "Piecemeal",                matrix(c(1,0,0,1,0,1,0,0,0), nrow = 3),
                       "Random motion of dots",    matrix(c(1,0,0,0,0,1,0,0,1), nrow = 3),
                       "Superimposed",             matrix(c(1,0,0,0,1,1,0,0,0), nrow = 3),
                       "Superimposed dispersing/gathering",  matrix(c(1,1,0,1,1,0,0,0,0), nrow = 3),
                       "Superimposed halves",      matrix(c(1,0,0,1,1,0,1,0,0), nrow = 3),
                       "Superimposed piecemeal",   matrix(c(1,0,0,1,1,1,0,0,0), nrow = 3),
                       "Superimposed traveling wave",  matrix(c(1,1,0,1,1,0,1,0,0), nrow = 3),
                       "Traveling wave",           matrix(c(1,1,0,1,0,0,1,0,0), nrow = 3))
  # -------------------------------------------------------------------------- #
  res <- c()
  for(i = 1:length(input)){
    x_cat <- input[i]
    indx <- 1:length(dict_categories)
    indx <- indx[sapply(dict_categories,identical,x_cat)]
    if(length(indx)>0){
      if(is.character(x_cat)){
        res <- c(res, dict_categories[indx+1])
      }else{
        res <- c(res, dict_categories[indx-1])
      }
    }else{
      res <- c(res, NULL)
    }
  }

  return(res)
}
