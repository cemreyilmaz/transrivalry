# ---------------------------------------------------------------------------- #
#' Median of each transition parameter
#'
#' Median and median absolute deviation of frequency, duration and speed are
#' calculated.
#'
#' @param data data.frame -- the preprocessed transition data
#'
#' @return data.frame -- It contains the median values of each measure and their
#'     median absolute values.
#'
#' @importFrom stats aggregate
#' @importFrom stats mad
#' @importFrom stats median
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_csv_data(csv_path)
#' data <- predefine_immediate(data,0)
#' med_values <- median_transition(data)}
median_transition <- function(data){
  data$dynamism[data$dynamism=='NaN']<-NA
  data$dynamism[data$dynamism=='dynamism']<-'dynamic'
  med_duration <- stats::aggregate(x=data$duration,
                                   by=list(data$category_code),
                                   FUN=median, na.rm = TRUE)
  mad_duration <- stats::aggregate(x=data$duration,
                                   by=list(data$category_code),
                                   FUN=mad, na.rm = TRUE)
  med_frequency <- stats::aggregate(x=data$frequency,
                                    by=list(data$category_code),
                                    FUN=median, na.rm = TRUE)
  mad_frequency <- stats::aggregate(x=data$frequency,
                                    by=list(data$category_code),
                                    FUN=mad, na.rm = TRUE)
  med_speed <- stats::aggregate(x=data$speed,
                                by=list(data$category_code),
                                FUN=median, na.rm = TRUE)
  mad_speed <- stats::aggregate(x=data$speed,
                                by=list(data$category_code),
                                FUN=mad, na.rm = TRUE)
  N <- array(as.numeric(table(data$category_code)), dim = nrow(table(data$category_code)))
  meds <- cbind(med_frequency, mad_frequency[,2],
                med_duration[,2], mad_duration[,2], med_speed[,2], mad_speed[,2],N)
  colnames(meds) <- c('category_name','median_frequency', 'mad_f',
                      'median_duration','mad_d','median_speed','mad_s','N')
  return(meds)
}
# ---------------------------------------------------------------------------- #
