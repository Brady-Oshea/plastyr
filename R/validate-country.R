#' Validate that a country exists in the dataset
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param country A string specifying the country name
#'
#' @return Invisible NULL; throws an error if the country is not found
validate_country <- function(dat, country) {
  if (!is.character(country) || length(country) != 1) {
    stop("`country` must be a single string.")
  }
  if (!country %in% dat$country) {
    stop(paste0('"', country, '" was not found in the dataset. Check spelling and capitalization.'))
  }
  invisible(NULL)
}
