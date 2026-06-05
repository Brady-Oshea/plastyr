#' Get the top plastic polluting countries
#'
#' Returns the top N countries by total plastic collected,
#' optionally filtered by year.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param n Number of top countries to return. Defaults to 10.
#' @param year Optional numeric year to filter by. If NULL, uses all years.
#'
#' @return A tibble with country and total_plastic columns, sorted descending.
#' @export
#'
#' @examples
#' plastic <- load_data()
#' top_polluters(plastic, n = 5)
top_polluters <- function(dat, n = 10, year = NULL) {
  if (!is.null(year)) {
    dat <- dplyr::filter(dat, .data[["year"]] == year)
  }
  dat |>
    dplyr::filter(!is.na(grand_total), grand_total > 0,
                  .data[["country"]] != "EMPTY") |>
    dplyr::group_by(.data[["country"]]) |>
    dplyr::summarize(total_plastic = sum(grand_total, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_plastic)) |>
    dplyr::slice_head(n = n)
}
