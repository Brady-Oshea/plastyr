#'
#' Computes country-level plastic totals from company rows and runs a paired
#' t-test on countries observed in both years.
#'
#' @param dat A data frame returned by \code{load_data()}.
#' @param year_before Numeric year to use as the baseline. Defaults to 2019.
#' @param year_after Numeric year to compare against the baseline. Defaults to 2020.
#'
#' @return A named list with the paired t-test object, a summary tibble, and a
#'   tibble of country-level changes.
#' @export
#'
#' @examples
#' plastic <- load_data()
#' plastic_year_change_test(plastic)
plastic_year_change_test <- function(dat, year_before = 2019, year_after = 2020) {
  if (!is.numeric(year_before) || length(year_before) != 1 ||
      !is.numeric(year_after) || length(year_after) != 1) {
    stop("year_before and year_after must each be a single numeric year.")
  }
  if (year_before == year_after) {
    stop("year_before and year_after must be different years.")
  }
  country_totals <- dat |>
    dplyr::filter(
      is.na(.data[["parent_company"]]) | .data[["parent_company"]] != "Grand Total",
      !is.na(.data[["country"]]),
      .data[["country"]] != "EMPTY",
      .data[["year"]] %in% c(year_before, year_after),
      !is.na(.data[["grand_total"]]),
      .data[["grand_total"]] >= 0
    ) |>
    dplyr::group_by(.data[["country"]], .data[["year"]]) |>
    dplyr::summarize(total = sum(.data[["grand_total"]], na.rm = TRUE), .groups = "drop")
  before <- country_totals |>
    dplyr::filter(.data[["year"]] == year_before) |>
    dplyr::transmute(country = .data[["country"]], total_before = .data[["total"]])
  after <- country_totals |>
    dplyr::filter(.data[["year"]] == year_after) |>
    dplyr::transmute(country = .data[["country"]], total_after = .data[["total"]])
  country_changes <- dplyr::inner_join(before, after, by = "country") |>
    dplyr::filter(.data[["total_before"]] > 0) |>
    dplyr::mutate(
      change = .data[["total_after"]] - .data[["total_before"]],
      percent_change = .data[["change"]] / .data[["total_before"]] * 100
    ) |>
    dplyr::arrange(dplyr::desc(abs(.data[["change"]])))
  test <- stats::t.test(
    x = country_changes$total_after,
    y = country_changes$total_before,
    paired = TRUE
  )
  summary <- country_changes |>
    dplyr::summarize(
      n_countries = dplyr::n(),
      total_before = sum(.data[["total_before"]], na.rm = TRUE),
      total_after = sum(.data[["total_after"]], na.rm = TRUE),
      total_change = sum(.data[["change"]], na.rm = TRUE),
      percent_total_change = sum(.data[["change"]], na.rm = TRUE) /
        sum(.data[["total_before"]], na.rm = TRUE) * 100,
      mean_change = mean(.data[["change"]], na.rm = TRUE),
      median_change = stats::median(.data[["change"]], na.rm = TRUE),
      mean_percent_change = mean(.data[["percent_change"]], na.rm = TRUE),
      median_percent_change = stats::median(.data[["percent_change"]], na.rm = TRUE),
      p_value = test$p.value
    )
  list(
    test = test,
    summary = summary,
    country_changes = country_changes
  )
}
