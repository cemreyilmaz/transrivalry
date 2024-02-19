#' Calculate several rivalry parameters for given data
#' This function calculates several statistical values for the rivalry data. It
#' is inspired by matlab function analyzeRivalry() created N. Zaretskaya (2015).
#'
#' @param data dataframe
#' @param subject char
#' @param session numeric
#' @param run numeric
#' @param trial numeric
#'
#' @return dataframe
#' @export
#'
#' @example
#' \dontrun{
#' curr_data <- riv.parameters(curr_trial,subject="s001",session=1,run=1,trial=1)}
riv.parameters <- function(data,subject,session,run,trial){
  mean_transition_dur  <- mean(na.omit(data$duration[data$dominance=="Transition"]))
  std_transition_dur   <- stats::sd(na.omit(data$duration[data$dominance=="Transition"]))
  med_transition_dur   <- stats::median(na.omit(data$duration[data$dominance=="Transition"]))
  total_transition_dur <- sum(na.omit(data$duration[data$dominance=="Transition"]))
  mean_dom_dur         <- mean(na.omit(data$duration[data$dominance=="Dominance"]))
  std_dom_dur          <- stats::sd(na.omit(data$duration[data$dominance=="Dominance"]))
  med_dom_dur          <- stats::median(na.omit(data$duration[data$dominance=="Dominance"]))
  total_dom_dur        <- sum(na.omit(data$duration[data$dominance=="Dominance"]))
  mean_leftarrow_dur   <- mean(na.omit(data$duration[data$keyname=="LeftArrow"]))
  std_leftarrow_dur    <- stats::sd(na.omit(data$duration[data$keyname=="LeftArrow"]))
  med_leftarrow_dur    <- stats::median(na.omit(data$duration[data$keyname=="LeftArrow"]))
  total_leftarrow_dur  <- sum(na.omit(data$duration[data$keyname=="LeftArrow"]))
  mean_rightarrow_dur  <- mean(na.omit(data$duration[data$keyname=="RightArrow"]))
  std_rightarrow_dur   <- stats::sd(na.omit(data$duration[data$keyname=="RightArrow"]))
  med_rightarrow_dur   <- stats::median(na.omit(data$duration[data$keyname=="RightArrow"]))
  total_rightarrow_dur <- sum(na.omit(data$duration[data$keyname=="RightArrow"]))
  transition_rate      <- length(data$duration[data$dominance=="Transition"]) / length(data$duration)
  dom_rate             <- length(data$duration[data$dominance=="Dominance"]) / length(data$duration)
  reversal_eye_rate    <- length(data$duration[data$reversal_eye=="Reversal_LeftEye" | data$reversal_eye=="Reversal_RightEye"]) / length(data$duration)
  curr_data <- data.frame(subject=subject,session=session,run=run,trial=trial,
                          avg_trans_dur=mean_transition_dur,
                          avg_dom_dur=mean_dom_dur,
                          avg_leftarrow=mean_leftarrow_dur,
                          avg_rightarrow=mean_rightarrow_dur,
                          std_trans_dur=std_transition_dur,
                          std_dom_dur=std_dom_dur,
                          std_leftarrow=std_leftarrow_dur,
                          std_rightarrow=std_rightarrow_dur,  
                          median_trans_dur=med_transition_dur,
                          median_dom_dur=med_dom_dur,
                          median_leftarrow=med_leftarrow_dur,
                          median_rightarrow=med_rightarrow_dur,
                          total_trans_dur=total_transition_dur,
                          total_dom_dur=total_dom_dur,
                          total_leftarrow=total_leftarrow_dur,
                          total_rightarrow=total_rightarrow_dur,
                          trans_rate=transition_rate,
                          dom_rate=dom_rate,
                          reversal_eye_rate=reversal_eye_rate)
  return(curr_data)
}