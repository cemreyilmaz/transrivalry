# ---------------------------------------------------------------------------- #
#' Reading the mat file containing the questionnaire data and demographics
#'
#' This functions reads the mat files created in matlab. The mat files include
#' the demographic data which is collected on a survey and the quantitative data
#' of the transition questionnaire. Those data are combined on matlab and saved
#' as one mat file per subject.
#'
#' @param filelocation char -- the path of the file including the file name and
#'     extension. The file is created on matlab and includes the questionnaires
#'     for each run and the demographic info about the subject.
#' @note The function warns with a message if the file doesn't exist or cannot
#'     be read.
#' @return list
#' @importFrom R.matlab readMat
#' @export
#'
#' @examples
#' \dontrun{
#' read_mat_data('Users/username/Documents/s001_assessment.mat')}
read_mat_data <- function(filelocation){
  tryCatch({
    data  <- R.matlab::readMat(filelocation)
  }, error = function(e){
    warning('The file cannot be read!')
    return(NULL)
  })
  subjectID <- data[['subjectID']]
  tmp <- data[[1]]
  personal_info <- matrix(NA, nrow = length(tmp)/2, ncol = 2)
  for(i in 1:(length(tmp)/2)){
    personal_info[i,] <- c(unlist(tmp[i]),unlist(tmp[i+length(tmp)/2]))
  }
  tmp <- data[[2]]
  for(i in 1:length(tmp)){
    if(!length(unlist(tmp[[i]]))){
      tmp[[i]] <- 'NA'
    }
  }
  questionnaire <- matrix(NA, nrow = length(tmp)/7, ncol = 7)
  ind_tmp <- 1
  for(i in 1:dim(questionnaire)[1]){
    questionnaire[i,] <- c(unlist(tmp[ind_tmp]),unlist(tmp[ind_tmp+1]),unlist(tmp[ind_tmp+2]),
                           unlist(tmp[ind_tmp+3]),unlist(tmp[ind_tmp+4]),unlist(tmp[ind_tmp+5]),
                           unlist(tmp[ind_tmp+6]))
    ind_tmp <- ind_tmp+7
  }
  output <- list(personal_info,questionnaire,subjectID)
}
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Combining the csv and mat data into csv file for the given subjects
#'
#' This function adds the demographic data and the quantitative data of
#' transition questionnaire by using \link{combine_csv_mat} for every subject.
#'
#' @param csv_path character -- the fullpath of csv file. If you have a variable
#'     holding the transition data, you don't have to define csv path.
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
#' @note It uses \link{combine_csv_mat} function.
#' @note It does not change the file. If the file is wanted to be changed,
#'     one must save the output as csv.
#' @importFrom utils write.csv
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- combine_all_subjects(csv_path,mat_folder,subject_list,'assessments_',1)
#' write.csv(data, file = csv_path) # to save onto the csv file}
combine_all_subjects <- function(csv_path,mat_folder,subject_list,basename = "assessments_",after_basename = 1,data){
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

  }
  return(output)
}
# ---------------------------------------------------------------------------- #
#' Defining the category as immediate when there is no mixed percept
#'
#' This function defines the category of transition as 'immediate' with the
#' category code 0 when the mixed percept is absent ('no' in mixed_percept
#' column).
#'
#' @param data data.frame -- the output of preprocessing steps
#' @param save_flag logical -- TRUE if you want to save the output as csv
#' @param csv_path character -- where to write the output csv file
#' @note If save_flag and csv_path variables are defined, the function saves the
#'     output onto the csv file. If not, the output can be used only in the
#'     current session as defined variable.
#' @return data.frame -- same data structure with added 'immediate' categories
#' @export
#'
#' @examples
#' \dontrun{
#' data <- predefine_immediate(data)
#' predefine_immediate(data, 1, csv_path) # to save onto the csv file}
predefine_immediate <- function(data, save_flag, csv_path){
  if(missing(save_flag)){
    save_flag <- 0
  }
  if(missing(csv_path)){
    csv_path <- getwd()
  }
  mixedpercept <- data['mixed_percept']
  data[mixedpercept == 'no', 'category_code'] <- 0
  data[mixedpercept == 'no', 'category_name'] <- 'immediate'
  if(save_flag){
    utils::write.csv(data,csv_path,row.names = F)
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
