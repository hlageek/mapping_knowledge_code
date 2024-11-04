#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param people_top_profiles
#' @param conversion_table
#' @param isvavai_data
#' @return
#' @author hlageek
#' @export
calc_supp_vars <- function(people_data, pca_disc_ind, pca_projection, conversion_table, isvavai_data) {
  library(tidytable)
  library(ggplot2)

#   supp_vars <- people_data |>
#     select(vedidk, freq_ford, gender) |>
#     left_join(conversion_table) |>
#     # PIs
#     left_join(
#       arrow::read_feather(isvavai_data$cep_investigators$path) |>
#         filter(role_researcher == "G" & role_institution == "primary") |>
#         select(vedidk) |>
#         mutate(pi_status = TRUE) |>
#         filter(!duplicated(vedidk))
#     ) |>
#     mutate(pi_status = ifelse(is.na(pi_status), "non-PI", "PI"))

#   pi_profiles <- supp_vars |>
#     na.omit() |>
#     left_join(people_data |> select(vedidk, starts_with("topic")), by = "vedidk") |>
#     summarise(across(
#       starts_with("topic"),
#       \(x) mean(x)
#     ), .by = c(freq_field, freq_ford, pi_status))

  gender_profiles <- people_data |>
    select(vedidk, freq_ford, gender, starts_with("topic")) |>
    na.omit() |>
    left_join(conversion_table) |>
    summarise(across(
      starts_with("topic"),
      \(x) mean(x)
    ), .by = c(freq_field, freq_ford, gender))

  pi_profiles_mat <- pi_profiles |>
    select(starts_with("topic")) |>
    as.matrix()
  gender_profiles_mat <- gender_profiles |>
    select(starts_with("topic")) |>
    as.matrix()

  mat_pca <- pca_disc_ind |>
    tidytable::select(starts_with("PC")) |>
    tidytable::select(tidyselect::all_of(c(1:3))) |>
    as.matrix()

  res_gender <- gender_profiles_mat %*% mat_pca |> as_tidytable()
  res_pi <- pi_profiles_mat %*% mat_pca |> as_tidytable()
  final_gender <- bind_cols(
    gender_profiles |> select(-starts_with("topic")),
    res_gender
  )

  final_pi <- bind_cols(
    pi_profiles |> select(-starts_with("topic")),
    res_pi
  )

  female_data <- final_gender %>%
    filter(gender == "female") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(start_PC1 = PC1, start_PC2 = PC2)

  male_data <- final_gender %>%
    filter(gender == "male") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(end_PC1 = PC1, end_PC2 = PC2)

  pi_data <- final_pi %>%
    filter(pi_status == "PI") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(end_PC1 = PC1, end_PC2 = PC2)
  non_pi_data <- final_pi %>%
    filter(pi_status == "non-PI") %>%
    select(freq_field, freq_ford, PC1, PC2) %>%
    rename(start_PC1 = PC1, start_PC2 = PC2)


  arrow_data_gender <- inner_join(female_data, male_data, by = c("freq_field", "freq_ford"))
  arrow_data_pi <- inner_join(pi_data, non_pi_data, by = c("freq_field", "freq_ford"))

  pca_projection |>
    select(vedidk, gender, pubs_total, freq_ford, starts_with("PC")) |>
    left_join(conversion_table) |>
    mutate(freq_ford = abbrev_discs(freq_ford)) |>
    tidytable::filter(pubs_total >= 5, !is.na(gender)) |>
    ggplot(aes(PC1, PC2)) +
    geom_point(color = "lightgrey", size = 0.5, alpha = 0.3) +
    geom_segment(
      data = arrow_data_gender,
      aes(x = start_PC1, y = start_PC2, xend = end_PC1, yend = end_PC2, color = freq_field),
      arrow = arrow(
        type = "closed",
        length = unit(0.01, "npc")
      ),
      size = 1.5
    ) +
    geom_point(
      data = arrow_data_gender,
      aes(x = start_PC1, y = start_PC2, color = freq_field),
      shape = 21, size = 3.5, fill = "white"
    ) +
    ggrepel::geom_text_repel(
      data = arrow_data_gender |> mutate(freq_ford = abbrev_discs(freq_ford)),
      aes(x = (start_PC1 + end_PC1) / 2, y = (start_PC2 + end_PC2) / 2, label = freq_ford), force = 5, force_pull = 0, max.overlaps = 20, seed = 12345, alpha = 1
    ) +
    # geom_point(data = final, aes(PC1, PC2, color = freq_ford, shape = gender, group = freq_field), size = 4, alpha = 1) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(color = "black")
    )

  pca_projection |>
    select(vedidk, pubs_total, freq_ford, starts_with("PC")) |>
    left_join(conversion_table) |>
    tidytable::filter(pubs_total >= 5) |>
    ggplot(aes(PC1, PC2)) +
    geom_point(color = "grey", size = 0.5, alpha = 0.3) +
    geom_segment(
      data = arrow_data_pi,
      aes(x = start_PC1, y = start_PC2, xend = end_PC1, yend = end_PC2, color = freq_field),
      arrow = arrow(type = "closed", length = unit(0.01, "npc"))
    ) +

    # geom_point(data = final, aes(PC1, PC2, color = freq_ford, shape = gender, group = freq_field), size = 4, alpha = 1) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(color = "black")
    )
}