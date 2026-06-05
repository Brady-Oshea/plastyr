#' Plot the top plastic polluting countries
#'
#' Creates a bar chart of the top N countries by total plastic collected.
#'
#' @param dat A data frame returned by \code{load_data()}
#' @param n Number of top countries to display. Defaults to 10.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plastic <- load_data()
#' plot_top_polluters(plastic)
plot_top_polluters <- function(dat, n = 10) {
  top <- top_polluters(dat, n = n)
  ggplot2::ggplot(top, ggplot2::aes(
    x = total_plastic,
    y = forcats::fct_reorder(.data[["country"]], total_plastic)
  )) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(
      title = paste("Top", n, "Plastic Polluting Countries"),
      x = "Total Plastic Collected",
      y = "Country"
    ) +
    ggplot2::theme_minimal()
}
