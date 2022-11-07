# ---------------------------------------------------------------------------- #
#' Mean of each transition parameter
#'
#' Mean and standard deviation of frequency, duration and speed are calculated.
#'
#' @param data data.frame -- the preprocessed transition data
#'
#' @return data.frame -- It contains the mean values of each measure and their
#'     standard deviation values.
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_csv_data(csv_path)
#' data <- predefine_immediate(data,0)
#' mean_values <- mean_transition(data)}
mean_transition <- function(data){
  data$dynamism[data$dynamism=='NaN']<-NA
  data$dynamism[data$dynamism=='dynamism']<-'dynamic'
  mean_duration <- stats::aggregate(x=data$duration,
                                    by=list(data$category_code),
                                    FUN=mean, na.rm = TRUE)
  std_duration <- stats::aggregate(x=data$duration,
                                   by=list(data$category_code),
                                   FUN=sd, na.rm = TRUE)
  mean_frequency <- stats::aggregate(x=data$frequency,
                                     by=list(data$category_code),
                                     FUN=mean, na.rm = TRUE)
  std_frequency <- stats::aggregate(x=data$frequency,
                                    by=list(data$category_code),
                                    FUN=sd, na.rm = TRUE)
  mean_speed <- stats::aggregate(x=data$speed,
                                 by=list(data$category_code),
                                 FUN=mean, na.rm = TRUE)
  std_speed <- stats::aggregate(x=data$speed,
                                by=list(data$category_code),
                                FUN=sd, na.rm = TRUE)
  N <- array(as.numeric(table(data$category_code)), dim = nrow(table(data$category_code)))
  means <- cbind(mean_frequency, std_frequency[,2],
                 mean_duration[,2], std_duration[,2], mean_speed[,2], std_speed[,2], N)
  colnames(means) <- c('category_code','mean_frequency', 'std_f',
                       'mean_duration','std_d','mean_speed','std_s', 'N')
  return(means)
}
# ---------------------------------------------------------------------------- #

