# ---------------------------------------------------------------------------- #
# The transition data were collected via two ways: a questionnaire on matlab and
# another one on paper. We need to combine the data of those two questionnaires
# and prepare the data file for analysis. The following functions are to prepare
# the data for analysis.
# ---------------------------------------------------------------------------- #
#' Reading the csv file containing the questionnaire data
#'
#' @param filelocation char -- the path of the file including the filename and
#'     extension. The file is assumed as csv file separated by tab and decimal
#'     point is defined as '.'.
#'
#' @note The function warns with a message if the file doesn't exist or cannot
#'     be read.
#'
#' @return data.frame -- the transition data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @example
#' \dontrun{
#' read_csv_data('/Users/username/Documents/questionnaire.csv')}
read_csv_data <- function(filelocation){
  tryCatch({
    data <- utils::read.csv(filelocation, sep=',', dec='.',
                            colClasses = c('numeric','character','character','numeric',
                                           'numeric','numeric','numeric','numeric','numeric',
                                           'numeric','character','numeric','numeric','character',
                                           'character','character','character','character',
                                           'character','character','numeric','numeric','numeric'))
  }, error = function(e1){
    tryCatch({
      data <- utils::read.csv(filelocation, sep=';', dec='.',
                              colClasses = c('numeric','character','character','numeric',
                                             'numeric','numeric','numeric','numeric','numeric',
                                             'numeric','character','numeric','numeric','character',
                                             'character','character','character','character',
                                             'character','character','numeric','numeric','numeric'))
    }, error = function(e2){
      warning('The file cannot be read!')
      return(0)
    })
  })
}
# ---------------------------------------------------------------------------- #
#' Reading the mat file containing the questionnaire data of individual subject
#'
#' @param filelocation char -- the path of the file including the filename and
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
#' Combining the csv and mat data into csv file
#'
#' @param csv_path character -- the fullpath of csv file
#' @param mat_path character -- the fullpath of mat file
#' @note This function uses \link{read_csv_data} and \link{read_mat_data}
#'     functions while reading the files.
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' combine_csv_mat(csv_path,mat_path)}
combine_csv_mat <- function(csv_path,mat_path){
  # read csv file
  data <- read_csv_data(csv_path)
  # read behavioral data of given subject
  curr_subj_quest <- read_mat_data(mat_path)
  # ---------- take the questionnaire ---------- #
  questionnaire <- curr_subj_quest[[2]]
  questionnaire[questionnaire[,5]=='NA',5] <- 'NaN'
  questionnaire[questionnaire[,6]=='NA',6] <- 'NaN'
  questionnaire[questionnaire[,7]=='NA',7] <- 'NaN'
  # mixed perception or not
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),19] <- questionnaire[2:dim(questionnaire)[1],3]
  # dynamism
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),20] <- questionnaire[2:dim(questionnaire)[1],4]
  # frequency
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),21] <- as.numeric(questionnaire[2:dim(questionnaire)[1],5])
  # duration
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),22] <- as.numeric(questionnaire[2:dim(questionnaire)[1],6])
  # speed
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),23] <- as.numeric(questionnaire[2:dim(questionnaire)[1],7])
  # ---------- take the limesurvey ---------- #
  personal <- curr_subj_quest[[1]]
  personal[1,personal[1,]=='NA'] <- 'NaN'
  personal[4,personal[4,]=='NA'] <- 'NaN'
  personal[5,personal[5,]=='NA'] <- 'NaN'
  personal[6,personal[6,]=='NA'] <- 'NaN'
  personal[7,personal[7,]=='NA'] <- 'NaN'
  personal[8,personal[8,]=='NA'] <- 'NaN'
  personal[9,personal[9,]=='NA'] <- 'NaN'
  personal[10,personal[10,]=='NA'] <- 'NaN'
  # age
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),1] <- rep(as.numeric(personal[1,2]),)
  # sex
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),2] <- rep(personal[2,2],)
  # education
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),3] <- rep(personal[3,2],)
  # diopter_right
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),4] <- rep(as.numeric(personal[4,2]),)
  # diopter_left
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),5] <- rep(as.numeric(personal[5,2]),)
  # handedness
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),6] <- rep(as.numeric(personal[6,2]),)
  # logMAR_both
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),7] <- rep(as.numeric(personal[7,2]),)
  # logMAR_right
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),8] <- rep(as.numeric(personal[8,2]),)
  # logMAR_left
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),9] <- rep(as.numeric(personal[9,2]),)
  # stereoacuity
  data[data['subject_id']==as.character(curr_subj_quest[[3]]),10] <- rep(as.numeric(personal[10,2]),)
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Combining the csv and mat data into csv file for the given subjects
#'
#' @param csv_path character -- the fullpath of csv file
#' @param mat_folder character -- the path of folder containing all the mat files
#' @param subject_list character -- It can be an array containing all the
#'     subject ids such as c('s001','s002','s003')
#' @param basename character -- the basename of the mat files such as
#'     'assessments_'
#' @param after_basename logical -- The file name of mat file is created
#'     as paste0(basename,subject_id) if 1 and
#'     as paste0(subject_id,basename) if 0.
#' @note It uses \link{combine_csv_mat} function.
#' @importFrom utils write.csv
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- combine_all_subjects(csv_path,mat_folder,subject_list,'assessments_',1)}
combine_all_subjects <- function(csv_path,mat_folder,subject_list,basename,after_basename){
  if(missing(after_basename)){
    after_basename <- 1
  }
  for(s in 1:length(subject_list)){
    curr_subject <- subject_list[s]
    if(after_basename){
      filepath <- dir(mat_folder, full.names=T, pattern=paste0(basename,curr_subject))
    }else{
      filepath <- dir(mat_folder, full.names=T, pattern=paste0(curr_subject,basename))
    }
    data <- combine_csv_mat(csv_path,filepath)
    utils::write.csv(data,csv_path,row.names = F)
  }
  return(data)
}
# ---------------------------------------------------------------------------- #
#' Defining the category as immediate when there is no mixed percept
#'
#' @param data data.frame -- the output of preprocessing steps
#' @param save_flag logical -- TRUE if you want to save the output as csv
#' @param csv_path character -- where to write the output csv file
#'
#' @return data.frame -- same data dtructure with added 'immediate' categories
#' @export
#'
#' @examples
#' \dontrun{
#' data <- predefine_immediate(data)
#' predefine_immediate(data, 1, csv_path)}
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
