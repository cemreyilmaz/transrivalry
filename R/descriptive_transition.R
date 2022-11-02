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
  meds <- median_transition(data)
  means <- mean_transition(data)
  all_stats <- rbind(t(meds[,2:(length(meds)-1)]),t(means[,2:length(means)]))
  cats <- unique(
    dplyr::group_by(
      data.frame(codes = data$category_code,
                 names = data$category_name),codes))
  cats <- cats[order(cats$codes),]
  cats <- cats[!is.na(cats[,1]),]
  colnames(all_stats) <- cats$names
  all_stats <- data.frame(all_stats)
  return(all_stats)
}
# ---------------------------------------------------------------------------- #
