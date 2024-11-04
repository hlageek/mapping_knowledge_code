#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param people_data
#' @param pca_disc_ind
#' @param pca_projection
#' @param conversion_table
#' @param isvavai_data
#' @return
#' @author hlageek
#' @export
project_pi_disc <- function(people_data, pca_disc_ind, pca_projection,
                            conversion_table, isvavai_data) {

  pis <- arrow::read_feather(isvavai_data$cep_investigators$path) |> 
filter(role_researcher == "G" & role_institution == "primary") |> 
pull(vedidk)  |> unique()

  pi_profiles <- people_data |>
    mutate(pi_status = ifelse(vedidk %in% pis, "PI", "non-PI")) |> 
    select(vedidk, freq_ford, pi_status, starts_with("topic")) |>
    na.omit() |>
    left_join(conversion_table) |>
    summarise(across(
      starts_with("topic"),
      \(x) mean(x)
    ), .by = c(freq_field, freq_ford, pi_status))

  pi_profiles_mat <- pi_profiles |>
    select(starts_with("topic")) |>
    as.matrix()

  mat_pca <- pca_disc_ind |>
    tidytable::select(starts_with("PC")) |>
    as.matrix()

  res_pi <- pi_profiles_mat %*% mat_pca |> as_tidytable()

  final_pi <- bind_cols(
    pi_profiles |> select(-starts_with("topic")),
    res_pi
  )

  pi_data <- final_pi %>%
    filter(pi_status == "PI") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(start_PC1 = PC1, start_PC2 = PC2)

  non_pi_data <- final_pi %>%
    filter(pi_status == "non-PI") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(end_PC1 = PC1, end_PC2 = PC2)

  arrow_data_pi <- inner_join(pi_data, non_pi_data, by = c("freq_field", "freq_ford"))

  xmin <- min(pca_projection[["PC1"]])
  xmax <- max(pca_projection[["PC1"]])
  ymin <- min(pca_projection[["PC2"]])
  ymax <- max(pca_projection[["PC2"]])

  # http://tsitsul.in/blog/coloropt/ color choice blog
  my_palette <- c(
    "#fb49b0",
    "#ddb310",
    "#b51d14",
    "#00beff",
    "#4053d3",
    "#00b25d"
  )
  pca_projection |>
    select(vedidk, pubs_total, freq_ford, starts_with("PC")) |>
    mutate(pi_status = ifelse(vedidk %in% pis, "PI", "non-PI")) |> 
    left_join(conversion_table) |>
    mutate(freq_ford = abbrev_discs(freq_ford)) |>
    tidytable::filter(pubs_total >= 5, !is.na(pi_status)) |>
    ggplot(aes(PC1, PC2)) +
        geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    geom_point(color = "lightgrey", size = 0.1, alpha = 0.3) +
    geom_segment(
      data = arrow_data_pi,
      aes(x = start_PC1, y = start_PC2, xend = end_PC1, yend = end_PC2, color = freq_field),
      linewidth = 1.2, 
      lineend = "butt",
  linejoin = "mitre",
  show.legend = FALSE 
    ) +
    geom_point(
      data = arrow_data_pi,
      aes(x = end_PC1, y = end_PC2, color = freq_field, fill = freq_field),
      shape = 22, size = 2, show.legend = FALSE
    ) +
    geom_point(
      data = arrow_data_pi,
      aes(x = start_PC1, y = start_PC2, color = freq_field),
      shape = 21, size = 2, fill = "white"
    ) +
    ggrepel::geom_text_repel(
      data = arrow_data_pi |> mutate(freq_ford = abbrev_discs(freq_ford)),
      aes(x = (start_PC1 + end_PC1) / 2, y = (start_PC2 + end_PC2) / 2, label = freq_ford), 
      #force = 5, force_pull = -0.5,
      max.overlaps = 20, 
      min.segment.length = 1.5,
      size = 2.1,
      seed = 123
    ) +
    geom_rect(xmin = -0.5, xmax = 0.5, ymin = ymin, ymax = -3.2, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = -0.55, xmax = 0.55, ymin = 3.55, ymax = ymax, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = xmin, xmax = -5.05, ymin = -0.5, ymax = 0.5, fill = "white", linewidth = 0.2, color = "black") +
    geom_rect(xmin = 4, xmax = xmax, ymin = -0.5, ymax = 0.5, fill = "white", linewidth = 0.2, color = "black") +
    annotate("text", x = 0, y = ymax, label = "Non-life", vjust = 1.8, hjust = 0.5) +
    annotate("text", x = 0, y = ymin, label = "Life", vjust = -0.9, hjust = 0.5) +
    annotate("text", x = xmin, y = 0, label = "Culture", hjust = 0.5, vjust = 1.5, angle = 90) +
    annotate("text", x = xmax, y = 0, label = "Nature", hjust = 0.5, vjust = -1, angle = 90) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = NA) +
    scale_color_manual(values = my_palette) +
    scale_fill_manual(values = my_palette, na.value = scales::alpha("white", 0.5)) +
        geom_point(aes(x=0,y=0, shape = pi_status), alpha = 0) +
scale_shape_manual(name = "PI status", 
                       values = c(24, 21), 
                       labels = c("non-PI", "PI")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.direction = "vertical",
      axis.text = element_text(color = "black")
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 2, override.aes = list(shape = 15, size = 3, alpha = 1)),
      shape = guide_legend(order = 2, nrow = 2, override.aes = list(size = 3, shape = c(1, 15), fill = "black", alpha = 1))
    )

}
