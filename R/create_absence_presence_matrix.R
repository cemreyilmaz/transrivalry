#' create absence/presence matrix for Jaccard Index calculation
#'
#' @param s character array
#' @param c character array
#' @param d data.frame
#' @param ss numeric
#'
#' @return matrix, array
#' @export
#'
#' @example
#' \dontrun{
#' stimuli    <- c("Gratings","Images","Dots")
#' categories <- c("halves","superimposed","piecemeal","travelling wave")
#' data       <- data.frame(stimulus_type = stimuli, 
#'                          session_code = c(1,2), 
#'                          category_name = categories)
#' presence.matrix <- create_absence_presence_matrix(stimuli,categories,data,sessionNo)}
create_absence_presence_matrix <- function(s,c,d,ss){
  abs_matrix <- c() # empty array for absence/presence matrix of session 1
  for(stim in s){ # for each stimulus type
    # extract stimulus data from session 1
    curr_d  <- d$category_name[d$stimulus_type==stim & d$session_code==ss]
    # absence/presence array for current data
    abs_array                <- rep(0,length(c))
    abs_array[c %in% curr_d] <- 1
    # merge absence/presence arrays of stimulus types
    if(length(abs_matrix)==0){
      abs_matrix <- abs_array
    } else{
      abs_matrix <- c(abs_matrix,t(abs_array))
    }
  }
  # convert the array into matrix
  abs_matrix           <- matrix(abs_matrix, nrow = length(c), ncol = length(s))
  colnames(abs_matrix) <- s
  rownames(abs_matrix) <- c
  return(abs_matrix)
}