#' Summarize plastic type proportions for a country
#'
#' Returns the proportion of each plastic type out of the grand total
#' for a given country, averaged across all years.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param country A string specifying the country name. Defaults to "United States".
#'
#' @return A tibble with columns for each plastic type and their proportions
#' @export
country_plastic_prop <- function(dat, country = "United States of America") {

  validate_country(dat, country)

  dat |>
    dplyr::filter(.data[["country"]] == country,
                  !is.na(grand_total),
                  grand_total > 0) |>
    dplyr::summarize(
      hdpe = sum(hdpe, na.rm = TRUE),
      ldpe = sum(ldpe, na.rm = TRUE),
      o    = sum(o,    na.rm = TRUE),
      pet  = sum(pet,  na.rm = TRUE),
      pp   = sum(pp,   na.rm = TRUE),
      ps   = sum(ps,   na.rm = TRUE),
      pvc  = sum(pvc,  na.rm = TRUE),
      total = sum(grand_total, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      hdpe = hdpe / total,
      ldpe = ldpe / total,
      o    = o    / total,
      pet  = pet  / total,
      pp   = pp   / total,
      ps   = ps   / total,
      pvc  = pvc  / total
    ) |>
    dplyr::select(-total)
}
