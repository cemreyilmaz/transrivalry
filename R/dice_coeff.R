#' Sorensen-Dice Coefficient
#'
#' @param matrix1 matrix, array
#' @param matrix2 matrix, array
#'
#' @return numeric
#' @export
#'
#' @examples /dontrun{dice_coeff(matrix1,matrix2)}
dice_coeff <- function(matrix1,matrix2){
  J <- jaccard.index(matrix1,matrix2)
  S <- 2*J/(1+J)
  return(S)
}
