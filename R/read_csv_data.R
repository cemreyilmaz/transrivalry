# ---------------------------------------------------------------------------- #
#' Reading the csv file containing the questionnaire data
#'
#' This functions reads the csv file containing all the questionnaire data.
#'
#' This csv file is created by the manual examination of each questionnaire.
#' During this process, the transition appearances are categorized by content
#' analysis for the drawings and description text drawn and written by the
#' participants.
#'
#' The demographics and the quantitative data of the questionnaire are added
#' later by the function \link{combine_csv_mat}.
#'
#' @param filelocation char -- the path of the file including the filename and
#'     extension. The file is assumed as csv file separated by comma ',' or
#'     semicomma ';' and decimal point is defined as '.'.
#'
#' @note The function warns with a message if the file doesn't exist or cannot
#'     be read.
#'
#' @return data.frame -- the transition data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
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
