#' Fit a linear model of plastic totals over time
#'
#' Fits a simple linear regression of total plastic collected vs. year
#' for a given country.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param country_name A string specifying the country. Defaults to "United States Of America".
#'
#' @return A named list with the lm model object and a tidy summary tibble.
#' @importFrom stats lm
#' @export
#'
#' @examples
#' plastic <- load_data()
#' plastic_lm(plastic, country_name = "China")
plastic_lm <- function(dat, country_name = "United States Of America") {
  country_dat <- dat |>
    dplyr::filter(.data[["country"]] == country_name,
                  !is.na(grand_total), grand_total > 0) |>
    dplyr::group_by(.data[["year"]]) |>
    dplyr::summarize(total = sum(grand_total, na.rm = TRUE), .groups = "drop")

  if (nrow(country_dat) < 2) {
    stop(paste("Not enough data to fit a model for:", country_name))
  }

  model <- lm(total ~ year, data = country_dat)
  list(
    model   = model,
    summary = broom::tidy(model)
  )
}
