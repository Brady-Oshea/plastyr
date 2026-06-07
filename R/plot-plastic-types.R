#' Plot plastic type breakdown for a country
#'
#' Creates a bar chart showing the count of each plastic type
#' for a given country.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param country_name A string specifying the country. Defaults to "China".
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plastic <- load_data()
#' plot_plastic_types(plastic, country_name = "China")
plot_plastic_types <- function(dat, country_name = "China") {
  dat |>
    dplyr::filter(.data[["country"]] == country_name,
                  !is.na(grand_total), grand_total > 0) |>
    dplyr::summarize(
      hdpe = sum(hdpe, na.rm = TRUE),
      ldpe = sum(ldpe, na.rm = TRUE),
      o    = sum(o,    na.rm = TRUE),
      pet  = sum(pet,  na.rm = TRUE),
      pp   = sum(pp,   na.rm = TRUE),
      ps   = sum(ps,   na.rm = TRUE),
      pvc  = sum(pvc,  na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(tidyr::everything(),
                        names_to = "type",
                        values_to = "count") |>
    ggplot2::ggplot(ggplot2::aes(x = type, y = count, fill = type)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = paste("Plastic Types for", country_name),
      x = "Plastic Type",
      y = "Total Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
