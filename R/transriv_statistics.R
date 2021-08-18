# ---------------------------------------------------------------------------- #
# Basic statistics after preprocessing
# ---------------------------------------------------------------------------- #

# While developing the code, easy start:
# subj_list <- c('s001','s002','s003','s005','s006','s007')
# csv_path <- '/Users/yilmaz/Documents/R/transrivalry/tests/data.csv'
# mat_folder <- '/Users/yilmaz/Documents/R/transrivalry/tests'
# basename <- 'assessments_'
# combine_all_subjects <- function(csv_path,mat_folder,subject_list,basename,after_basename)
# data <- transrivalry::read_csv_data(csv_path)
# data['category_code'] <- rep(c(1,1,1,2,2,2,3,3,4,5),79)
# data <- transrivalry::predefine_immediate(data,0)

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
  med_duration <- stats::aggregate(x=data[,'duration'],
                           by=list(data[,'category_code']),
                           FUN=median, na.rm = TRUE)
  mad_duration <- stats::aggregate(x=data[,'duration'],
                                   by=list(data[,'category_code']),
                                   FUN=mad, na.rm = TRUE)
  med_frequency <- stats::aggregate(x=data[,'frequency'],
                           by=list(data[,'category_code']),
                           FUN=median, na.rm = TRUE)
  mad_frequency <- stats::aggregate(x=data[,'frequency'],
                                    by=list(data[,'category_code']),
                                    FUN=mad, na.rm = TRUE)
  med_speed <- stats::aggregate(x=data[,'speed'],
                                by=list(data[,'category_code']),
                                FUN=median, na.rm = TRUE)
  mad_speed <- stats::aggregate(x=data[,'speed'],
                                by=list(data[,'category_code']),
                                FUN=mad, na.rm = TRUE)
  N <- array(as.numeric(table(data$category_code)), dim = nrow(table(data$category_code)))
  meds <- cbind(med_frequency, mad_frequency[,2],
                med_duration[,2], mad_duration[,2], med_speed[,2], mad_speed[,2],N)
  colnames(meds) <- c('category_code','median_frequency', 'mad_f',
                      'median_duration','mad_d','median_speed','mad_s','N')
  return(meds)
}
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
  mean_duration <- stats::aggregate(x=data[,'duration'],
                                   by=list(data[,'category_code']),
                                   FUN=mean, na.rm = TRUE)
  std_duration <- stats::aggregate(x=data[,'duration'],
                                   by=list(data[,'category_code']),
                                   FUN=sd, na.rm = TRUE)
  mean_frequency <- stats::aggregate(x=data[,'frequency'],
                                    by=list(data[,'category_code']),
                                    FUN=mean, na.rm = TRUE)
  std_frequency <- stats::aggregate(x=data[,'frequency'],
                                    by=list(data[,'category_code']),
                                    FUN=sd, na.rm = TRUE)
  mean_speed <- stats::aggregate(x=data[,'speed'],
                                by=list(data[,'category_code']),
                                FUN=mean, na.rm = TRUE)
  std_speed <- stats::aggregate(x=data[,'speed'],
                                by=list(data[,'category_code']),
                                FUN=sd, na.rm = TRUE)
  N <- array(as.numeric(table(data$category_code)), dim = nrow(table(data$category_code)))
  means <- cbind(mean_frequency, std_frequency[,2],
                mean_duration[,2], std_duration[,2], mean_speed[,2], std_speed[,2], N)
  colnames(means) <- c('category_code','mean_frequency', 'std_f',
                      'mean_duration','std_d','mean_speed','std_s', 'N')
  return(means)
}
# ---------------------------------------------------------------------------- #
#' Descriptive statistics of transition parameters
#'
#' Before calculating the statistics, the category without a mixed percept is
#' defined by using \link{predefine_immediate} function. Then, the
#' mean, standard deviation, median, median absolute deviation of frequency,
#' duration and speed are calculated for every transition types.
#'
#' @param data data.frame -- preprocessed transition data
#'
#' @return data.frame -- descriptive statistics (median, mad and mean, std)
#' @export
#'
#' @examples
#' \dontrun{
#' all_stats <- descriptive_transition(data)}
descriptive_transition <- function(data){
  data <- predefine_immediate(data,0)
  meds <- median_transition(data)
  means <- mean_transition(data)
  all_stats <- rbind(t(meds[,2:(length(meds)-1)]),t(means[,2:length(means)]))
  colnames(all_stats) <- paste0('category_',0:(dim(all_stats)[2]-1))
  all_stats <- data.frame(all_stats)
  return(all_stats)
}
# ---------------------------------------------------------------------------- #
#' Normalize frequency in each run
#'
#' This function divides each frequency value by the sum of frequencies in the
#' corresponding run so that all the transition types have normalized and
#' comparable relative frequencies.
#'
#' @param data data.frame -- preprocessed transition data
#'
#' @return data.frame -- transition data with normalized frequency values
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_csv_data(csv_path)
#' data <- predefine_immediate(data,0)
#' data <- normalize_frequency(data)}
normalize_frequency <- function(data){
  subj <- unique(data$subject_id)
  ses <- unique(data$session_code)
  run <- unique(data$run_code)
  for(c_subj in subj){
    for(c_ses in ses){
      for(c_run in run){
        total_freq <- sum(data$frequency[data$subject_id==c_subj &
                                           data$session_code==c_ses &
                                           data$run_code==c_run])
        data$frequency[data$subject_id==c_subj &
                         data$session_code==c_ses &
                         data$run_code==c_run] <- data$frequency[data$subject_id==c_subj &
                                                                   data$session_code==c_ses &
                                                                   data$run_code==c_run] / total_freq
        data$frequency[data$subject_id==c_subj &
                         data$session_code==c_ses &
                         data$run_code==c_run] <- data$frequency[data$subject_id==c_subj &
                                                                   data$session_code==c_ses &
                                                                   data$run_code==c_run] * 100
      }
    }
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Convert the speed values
#'
#' The speed parameter was collected in just an opposite way during the
#' experiment. The maximum speed was recorded as 0 and the minimum was 100. This
#' function corrects the speed values.
#'
#' @param data data.frame -- preprocessed transition data
#'
#' @return data.frame -- transition data with corrected speed values
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_csv_data(csv_path)
#' data <- predefine_immediate(data,0)
#' data <- convert_speed(data)}
convert_speed <- function(data){
  subj <- unique(data$subject_id)
  ses <- unique(data$session_code)
  run <- unique(data$run_code)
  for(c_subj in subj){
    for(c_ses in ses){
      for(c_run in run){
        data$speed[data$subject_id==c_subj &
                     data$session_code==c_ses &
                     data$run_code==c_run] <- 100 - data$speed[data$subject_id==c_subj &
                                                                 data$session_code==c_ses &
                                                                 data$run_code==c_run]
      }
    }
  }
  return(data)
}
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
