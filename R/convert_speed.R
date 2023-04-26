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
        tmp <- data$speed[data$subject_id==c_subj & data$session_code==c_ses &
                            data$run_code==c_run]
        tmp[!is.na(tmp)] <- 100 - tmp[!is.na(tmp)]
        data$speed[data$subject_id==c_subj & data$session_code==c_ses &
                     data$run_code==c_run] <- tmp
      }
    }
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
