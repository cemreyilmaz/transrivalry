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
