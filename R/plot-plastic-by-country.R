#' Plot top n countries by total plastic pollution
#'
#' Creates a bar chart of the top n countries ranked by total plastic collected,
#' in descending order.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param top_n A positive integer specifying how many countries to show. Defaults to 10.
#'
#' @return A ggplot object
#' @export
plot_plastic_by_country <- function(dat, top_n = 10) {

  if (!is.numeric(top_n) || top_n < 1) {
    stop("`top_n` must be a positive number.")
  }

  top_n <- as.integer(top_n)

  plot_dat <- dat |>
    dplyr::filter(!is.na(grand_total), country != "EMPTY") |>
    dplyr::group_by(country) |>
    dplyr::summarize(total = sum(grand_total, na.rm = TRUE), .groups = "drop") |>
    dplyr::slice_max(order_by = total, n = top_n) |>
    dplyr::mutate(country = forcats::fct_reorder(country, total))

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = country, y = total)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top", top_n, "Countries by Total Plastic Pollution"),
      x = "Country",
      y = "Total Plastic Collected"
    ) +
    ggplot2::theme_minimal()
}
