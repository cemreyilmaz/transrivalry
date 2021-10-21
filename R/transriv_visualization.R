# ---------------------------------------------------------------------------- #
# Visualization of preprocessed data
# ---------------------------------------------------------------------------- #
#' Plot descriptive statistics with error bars
#'
#' This function can use the output of descriptive stat functions
#' \link{descriptive_transition}, \link{mean_transition} or
#' \link{median_transition} to visualize the results.
#'
#' @note The error bars are drawn as given in the stat_data. The default of this
#' package is standard deviation for mean and median absolute value for median.
#'
#' @param stat_data data.frame -- all statistical results of transition parameters
#'    The output of \link{descriptive_transition} can be used.
#' @param stat_val character -- either mean or median to extract data
#' @param trans_val character -- 'frequency', 'duration' or 'speed' to extract data
#'     If not defined, all parameters will be drawn on a plot.
#' @param cat_names character -- category names as a character array for x-label
#' @param convert_na logical -- whether or not the na values in statistics are
#'     desired to be converted into zero values
#' @return ggplot
#'
#' @import ggplot2
#'
#' @export
#' @examples
#' \dontrun{
#' plot_stats(all_stats,'median','frequency')
#' plot_stats(all_stats,'mean')
#' }
#'
plot_stats <- function(stat_data, stat_val, trans_val, cat_names, convert_na){
  if(!missing(stat_val)){ # if defined, extract only median or mean
    a <- dim(stat_data)[1]
    if(stat_val == 'median'){ # extract median
      stat_data <- rbind(stat_data[1:((a-1)/2),], stat_data[a,])
    }
    if(stat_val == 'mean'){ # extract mean
      stat_data <- rbind(stat_data[(((a-1)/2)+1):(a-1),], stat_data[a,])
    }
  }else{ # if not defined, we cannot continue...
    warning('Select either mean or median!')
  }
  if(!missing(trans_val)){ # if defined, extract only one parameter
    a <- rownames(stat_data)
    a <- grep(trans_val, a, fixed = TRUE)
    a <- c(a, a+1)
    stat_data <- rbind(stat_data[a,], stat_data[dim(stat_data)[1],])
  }else{ # if not defined, we cannot continue...
    warning('Select a transition parameter!')
  }
  if(missing(cat_names)){ # if not defined, use the labels in csv file as category names
    cat_names <- colnames(stat_data)
  }
  if(missing(convert_na)){ # if not defined, leave na values as it is
    convert_na <- 0
  }
  # prepare for plotting
  y_stats <- stat_data[1:2,] # all stats
  y_errors <- stat_data[3:4,] # all errors
  if(stat_val=='mean'){ # if mean
    if(trans_val=='frequency'){ # if frequency
      title_tex <- 'Mean Relative Frequency' # create title text
      y_tex <- 'Relative Frequency' # create y-label text
      y_stats <- as.matrix(y_stats[1,]) # reorganize mean values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[1,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
    if(trans_val=='duration'){ # if duration
      title_tex <- 'Mean Duration' # create title text
      y_tex <- 'Duration (sec)' # create y-label text
      y_stats <- as.matrix(y_stats[1,]) # reorganize mean values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[1,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
    if(trans_val=='speed'){ # if speed
      title_tex <- 'Mean Relative Speed' # create title text
      y_tex <- 'Relative Speed' # create y-label text
      y_stats <- as.matrix(y_stats[1,]) # reorganize mean values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[1,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
  }
  if(stat_val=='median'){ # if median
    if(trans_val=='frequency'){ # if frequency
      title_tex <- 'Median Relative Frequency' # create title text
      y_tex <- 'Relative Frequency' # create y-label text
      y_stats <- as.matrix(y_stats[2,]) # reorganize median values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[2,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
    if(trans_val=='duration'){ # if duration
      title_tex <- 'Median Duration' # create title text
      y_tex <- 'Duration (sec)' # create y-label text
      y_stats <- as.matrix(y_stats[2,]) # reorganize median values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[2,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
    if(trans_val=='speed'){
      title_tex <- 'Median Relative Speed' # create title text
      y_tex <- 'Relative Speed' # create y-label text
      y_stats <- as.matrix(y_stats[2,]) # reorganize median values
      rownames(y_stats) <- NULL
      colnames(y_stats) <- NULL
      y_errors <- as.matrix(y_errors[2,]) # reorganize error values
      rownames(y_errors) <- NULL
      colnames(y_errors) <- NULL
    }
  }
  # create data frame with stat and error values
  stat_data <- data.frame(stat_vals = t(y_stats), error_vals = t(y_errors))
  if(convert_na){ # if desired, na values can be changed into zeros
  stat_data[is.na(stat_data)] <- 0
  }
  # plot filtered data
  p <- ggplot2::ggplot(stat_data) +
    # first, mean/median
    ggplot2::geom_point(ggplot2::aes(x=cat_names, y=stat_vals),
                        size=1, colour='dimgray', stat='identity', alpha=.9) +
    # then, error bars
    ggplot2::geom_errorbar(ggplot2::aes(x=cat_names, ymin=stat_vals-error_vals,
                                       ymax=stat_vals+error_vals),
                           width=.2, colour='deepskyblue', alpha=.4, size=1.3) +
    # title and labels
    ggplot2::labs(title=title_tex, x='Categories', y=y_tex) +
    # x-label positioning
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=0))
  p
  return(p)
}
