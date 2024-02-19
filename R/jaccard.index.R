#' Jaccard Index 
#' 
#' This function calculates the Jaccard Index as following:
#'     1. Compare each cell of matrices individually. 
#'     2. Find the number of cells having the same content. (intersection)
#'     3. Calculate the total number of cells in two matrices.
#'     4. Subtract the intersection from the total number of cells.(union)
#'     5. Jaccard Index = intersection/union
#' The size of two matrices must be equal to calculate the Jaccard Index.
#' modified from: https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/
#'
#' @param a matrix, array
#' @param b matrix, array
#'
#' @return numeric
#' @export
#'
#' @example
#' \dontrun{
#' matrix1 <- c(23, 25, 27, 29)
#' matrix2 <- c(23, 25, 29, 31)
#' jaccard.index(matrix1, matrix2)}
jaccard.index <- function(a, b) {
  if(length(a) != length(b)){
    # first, check if the lengths are equal
    warning("The matrices have different sizes!")
    return(0)
  } else{
    intersection = sum(a==b)
    union = length(a) + length(b) - intersection
    return (intersection/union) 
  }
}