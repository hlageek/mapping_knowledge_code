#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param disc_clust_hc
#' @return
#' @author hlageek
#' @export
viz_disc_clustering <- function(disc_clust_hc, conversion_table) {
  dendro <- as.dendrogram(disc_clust_hc)

  my_palette <- c(
    "Agricultural and veterinary sciences"  = "#fb49b0",
    "Engineering and Technology" = "#ddb310",
    "Humanities and the Arts" = "#b51d14",
    "Medical and Health Sciences" = "#00beff",
    "Natural Sciences" = "#4053d3",
    "Social Sciences" ="#00b25d"
  )

  labels_df <- ggdendro::dendro_data(dendro)$labels |>
    left_join(
      conversion_table |>
        mutate(freq_ford = abbrev_discs(freq_ford)),
      by = c("label" = "freq_ford")
    ) |>
    mutate(label_color = case_when(
      freq_field == "Agricultural and veterinary sciences" ~ my_palette[1],
      freq_field == "Engineering and Technology" ~ my_palette[2],
      freq_field == "Humanities and the Arts" ~ my_palette[3],
      freq_field == "Medical and Health Sciences" ~ my_palette[4],
      freq_field == "Natural Sciences" ~ my_palette[5],
      freq_field == "Social Sciences" ~ my_palette[6]
    ))


  dendro_disc <- ggdendro::ggdendrogram(dendro, rotate = T, size = 2) +
    geom_point(data = labels_df, aes(x = 0, y = 0, color = freq_field), size = 0.01) +
    geom_point(aes(x = 0, y = 0), color = "white") +
    annotate("rect", xmin = 0.5, xmax = 15.5, ymin = 0, ymax = 150, colour = "darkgrey", alpha = 0, linetype = 2) +
    annotate("rect", xmin = 15.5, xmax = 25.5, ymin = 0, ymax = 150, colour = "darkgrey", alpha = 0, linetype = 3) +
    annotate("rect", xmin = 25.5, xmax = 42.5, ymin = 0, ymax = 150, colour = "darkgrey", alpha = 0, linetype = 4) +
    annotate("text", x = 8, y = 170, label = "Cluster 1", color = "black") +
    annotate("text", x = 22, y = 170, label = "Cluster 2A", color = "black") +
    annotate("text", x = 36, y = 170, label = "Cluster 2B", color = "black") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(values = my_palette) +
      theme(
      legend.position = c(0.4, 0.001),
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = ggtext::element_markdown(color = labels_df$label_color, size = 10),
legend.key = element_blank()    ) +
    guides(color = guide_legend(nrow = 2, override.aes = list(shape = 15, size = 3, alpha = 1)))
    


  dendro_disc
}
