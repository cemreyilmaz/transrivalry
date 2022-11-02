# ---------------------------------------------------------------------------- #
#' Combining the demographics and transition questionnaires
#'
#' This function combines the demographics and the quantitative data of 
#' transition questionnaire. 
#'
#' @param mat_path character -- the fullpath of mat files
#' @note This function uses \link{read_csv_data} and \link{read_mat_data}
#'     functions while reading the files.
#' @note Use this function only for one subject at a time!
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' curr_subj <- combine_csv_mat(mat_path)
#' all_subj  <- rbind(all_subj,curr_subj)
#' all_data  <- cbind(data, all_subj)}
combine_demog_trans <- function(csv_path,mat_path){
  # ---------- take the questionnaire ---------- #
  questionnaire <- curr_subj_quest[[2]]
  questionnaire[questionnaire[,5]=='NA',5] <- 'NaN'
  questionnaire[questionnaire[,6]=='NA',6] <- 'NaN'
  questionnaire[questionnaire[,7]=='NA',7] <- 'NaN'
  
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
  
  data <- data.frame( # age
    age = rep(as.numeric(personal[1,2]),dim(questionnaire)[1]-1),
    # sex
    sex = rep(personal[2,2],dim(questionnaire)[1]-1),
    # education
    edu = rep(personal[3,2],dim(questionnaire)[1]-1),
    # diopter_right
    r_diop = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # diopter_left
    l_diop = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # handedness
    hand = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # logMAR_both
    acu_both = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # logMAR_right
    acu_r = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # logMAR_left
    acu_l = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # stereoacuity
    stereo = rep(as.numeric(personal[4,2]),dim(questionnaire)[1]-1),
    # mixed perception or not
    mixed_percept = questionnaire[2:dim(questionnaire)[1],3],
    # dynamism
    dynamism = questionnaire[2:dim(questionnaire)[1],4],
    # frequency
    frequency = as.numeric(questionnaire[2:dim(questionnaire)[1],5]),
    # duration
    duration = as.numeric(questionnaire[2:dim(questionnaire)[1],6]),
    # speed
    speed = as.numeric(questionnaire[2:dim(questionnaire)[1],7]))
  return(data)
}
# ---------------------------------------------------------------------------- #
