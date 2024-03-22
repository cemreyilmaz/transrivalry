#' Draw presence plot
#'
#' @param curr_data data.frame
#'
#' @return ggplot
#' @export
#'
#' @examples
presence_plot <- function(curr_data,curr_title,upper_limit,x_labels = c('GG', 'II', 'DD', 'GI', 'DI', 'DG')){
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  title_size <- 10
  tick_size <- 14
  p1 <- ggplot() +
    geom_bar(data = curr_data,
             aes(x = stimulus.pairs, y = number),
             stat="identity") +
    ggtitle(curr_title) +
    scale_x_discrete(labels = x_labels) +
    theme(legend.position = "none", # hide legend
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "darkgray"),
          axis.title = element_blank(),
          title =element_text(size=title_size), plot.title = element_text(hjust = 0.5))
  if(!missing(upper_limit)){
    p1 <- p1 +
      scale_y_continuous(breaks = integer_breaks(), expand = c(0, 0), limits = c(0,upper_limit))
  } else{
    p1 <- p1 +
      scale_y_continuous(breaks = integer_breaks(), expand = c(0, 0))

  }
  return(p1)
}
