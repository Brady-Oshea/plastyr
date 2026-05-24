#' Loads in Plastics Data set
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' plastic <- load_data()

load_data <- function(){
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv')

}
