#' Load the Break Free From Plastic dataset
#'
#' @return A tibble with plastic pollution data from TidyTuesday 2021-01-26
#' @export
load_data <- function() {
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv',
    show_col_types = FALSE
  )
}
