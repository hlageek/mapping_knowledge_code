#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_projection
#' @return
#' @author hlageek
#' @export
viz_pca_projection_field <- function(pca_projection, min_pubs = 5, axis_1 = "PC1", axis_2 = "PC2", label_top = NULL, label_bottom = NULL, label_left = NULL, label_right = NULL) {
  require(ggplot2)

  # Ford main categories of disciplines
  ford_cats <- pca_projection |>
    select(freq_ford, freq_field) |>
    group_by(freq_ford, freq_field) |>
    summarize(n = n(), .by = freq_field) |>
    filter(n == max(n)) |>
    ungroup() |>
    select(-n) |>
    rename(freq_field2 = freq_field)

  centers <- pca_projection |>
    tidytable::filter(pubs_total >= min_pubs) |>
    tidytable::group_by(freq_ford) |>
    tidytable::summarize(center_x = mean(.data[[axis_1]]), center_y = mean(.data[[axis_2]])) |>
    tidytable::left_join(ford_cats) |>
    tidytable::mutate(freq_ford = abbrev_discs(freq_ford))

  # http://tsitsul.in/blog/coloropt/ color choice blog
  my_palette <- c(
    "#fb49b0",
    "#ddb310",
    "#b51d14",
    "#00beff",
    "#4053d3",
    "#00b25d"
  )

  xmin <- min(pca_projection[[axis_1]])
  xmax <- max(pca_projection[[axis_1]])
  ymin <- min(pca_projection[[axis_2]])
  ymax <- max(pca_projection[[axis_2]])

  plot <- pca_projection |>
    tidytable::filter(pubs_total >= min_pubs) |>
    tidytable::left_join(ford_cats) |>
    ggplot(aes(.data[[axis_1]], .data[[axis_2]], color = freq_field2)) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    geom_point(size = 0.1, alpha = 0.6) +
    ggrepel::geom_label_repel(
      data = centers,
      aes(
        x = center_x, y = center_y,
        label = freq_ford,
        fill = freq_field2,
      ),
      color = "black",
      alpha = 0.5,
      size = 2.3,
      fontface = "bold",
      max.overlaps = 20,
      show.legend = FALSE,
      seed = 12345
    ) +
    ggrepel::geom_label_repel(
      data = centers,
      aes(
        x = center_x, y = center_y,
        label = freq_ford,
        fill = NA,
      ),
      color = "black",
      size = 2.3,
      fontface = "bold",
      max.overlaps = 20,
      show.legend = FALSE,
      seed = 12345
    ) +
    geom_rect(xmin = -0.5, xmax = 0.5, ymin = ymin, ymax = -4, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = -0.5, xmax = 0.5, ymin = 2.5, ymax = ymax, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = xmin, xmax = -5.1, ymin = -0.5, ymax = 0.5, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = 4.05, xmax = xmax, ymin = -0.5, ymax = 0.5, fill = "white", linewidth = 0.2, color = "black") +
    annotate("text", x = 0, y = ymax, label = label_top, vjust = 2, hjust = 0.5) +
    annotate("text", x = 0, y = ymin, label = label_bottom, vjust = -1, hjust = 0.5) +
    annotate("text", x = xmin, y = 0, label = label_left, hjust = 0.5, vjust = 1.5, angle = 90) +
    annotate("text", x = xmax, y = 0, label = label_right, hjust = 0.5, vjust = -1, angle = 90) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA) +
    scale_color_manual(values = my_palette) +
    scale_fill_manual(values = my_palette, na.value = scales::alpha("white", 0.5)) +
    # labs(
    #   title = "Structure of research topics orientations",
    #   subtitle = "Researchers' topic portofolios projected on the principal components of the disciplinary space of topics"
    # ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text = element_text(color = "black")
    ) +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 3, alpha = 1)))
plot

  export_figures(plot, here::here("figures"), filename = paste("plot", axis_1, axis_2, sep = "_"))

  plot
}
