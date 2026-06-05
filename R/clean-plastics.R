#' Clean the plastics dataset
#'
#' Removes empty or invalid rows, standardizes country names,
#' and filters out aggregated "Grand Total" rows.
#'
#' @param dat A data frame returned by \code{load_data()}
#'
#' @return A cleaned data frame.
#' @export
#'
#' @examples
#' plastic <- load_data()
#' clean <- clean_plastics(plastic)
clean_plastics <- function(dat) {
  dat |>
    dplyr::filter(
      !is.na(country),
      country != "EMPTY",
      !is.na(grand_total),
      grand_total > 0
    ) |>
    dplyr::mutate(
      country = stringr::str_trim(country),
      country = stringr::str_to_title(country)
    )
}
