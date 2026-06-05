#' Loads in Plastics Data set
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' plastic <- load_data()

load_data <- function() {
  path <- system.file("extdata", "plastics.csv", package = "plastyr")
  if (path == "") {
    stop("Data file not found. Make sure plastyr is installed correctly.")
  }
  tibble::as_tibble(data.table::fread(path))
}
