#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_projection
#' @param isvavai_data
#' @return
#' @author hlageek
#' @export
viz_applied_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {


applied_pis <- arrow::read_feather(isvavai_data$cep_investigators$path) |> 
filter(role_researcher == "G" & role_institution == "primary") |> 
left_join(
arrow::read_feather(isvavai_data$cep_details$path) |> 
 filter(str_detect(provider, "TA")) |> 
 select(kod, provider)
) |> 
summarise(
  applied_pi = any(!is.na(provider)),
  .by = vedidk
) |> 
bind_rows(pca_projection |> select(vedidk)) |> 
filter(!duplicated(vedidk))  |> 
mutate(applied_pi = replace_na(applied_pi, FALSE))


pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(applied_pis) |> 
summarise(
    mean_applied_pct = mean(applied_pi, na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_applied_pct, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "PIs in applied research",
    x = pca_labs_x(),
    y = "Share of PIs",
    color = pca_labs_color()
  ) +
  npgTheme()


}
