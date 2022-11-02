# ---------------------------------------------------------------------------- #
#' Combining the demographics and questionnaire data into content analysis 
#' results for the given subjects
#'
#' This function adds the demographic data and the quantitative data of
#' transition questionnaire by using \link{combine_demog_trans} for every subject.
#'
#' @param mat_folder character -- the path of folder containing all the mat files
#' @param subject_list character -- It can be an array containing all the
#'     subject ids such as c('s001','s002','s003'). By default, it is all the
#'     subjects of mat files.
#' @param basename character -- the basename of the mat files such as
#'     'assessments_'
#' @param after_basename logical -- The file name of mat file is created
#'     as paste0(basename,subject_id) if 1 and
#'     as paste0(subject_id,basename) if 0.
#' @param data data.frame -- transition data (optional)
#' @note It uses \link{combine_demog_trans} function.
#' @note If one feeds the function with data variable, the output includes
#'     the given data and the organized data as combined.
#'     
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- combine_all_subjects(mat_folder,subject_list,'assessments_',1,data)
#' write.csv(data, file = csv_path) # to save onto the csv file}
combine_all_subjects <- function(mat_folder,subject_list,basename = "assessments_",after_basename = 1,data){
  if(missing(subject_list)){
    subject_list <- substr(dir(mat_folder, full.names=T, pattern=".mat"),19,22)
  }
  output <- data.frame()
  for(s in 1:length(subject_list)){
    curr_subject <- subject_list[s]
    if(after_basename){
      filepath <- dir(mat_folder, full.names=T, pattern=paste0(basename,curr_subject))
    }else{
      filepath <- dir(mat_folder, full.names=T, pattern=paste0(curr_subject,basename))
    }
    if(is.null(output)){
      output <- transrivalry::combine_demog_trans(mat_path = filepath)
    } else{
      tmp <- transrivalry::combine_demog_trans(mat_path = filepath)
      output <- rbind(output,tmp)
    }
    if(!missing(data)){
      output <- cbind(data,output)
    }
  }
  return(output)
}
# ---------------------------------------------------------------------------- #
