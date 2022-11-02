# ---------------------------------------------------------------------------- #
#' Make all the small adjustments and corrections for transition data
#'
#' @param data data.frame -- preprocessed transition data
#'
#' @return data.frame -- normalized & corrected transition data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- normalize_data(data)}
normalize_data <- function(data){
  data <- predefine_immediate(data)
  data <- normalize_frequency(data)
  data <- convert_speed(data)
}
# ---------------------------------------------------------------------------- #
