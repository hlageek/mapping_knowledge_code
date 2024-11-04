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
viz_pi_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

pis <- arrow::read_feather(isvavai_data$cep_investigators$path) |> 
filter(role_researcher == "G" & role_institution == "primary") |> 
select(vedidk) |> 
mutate(pi_status = "PI") |> 
bind_rows(pca_projection |> select(vedidk)) |> 
filter(!duplicated(vedidk))  |> 
mutate(pi_status = replace_na(pi_status, "nonPI"))

pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(pis) |>
summarise(
    mean_pi = mean(pi_status == "PI", na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_pi, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Principal Investigators",
    x = pca_labs_x(),
    y = "Share of PIs",
    color = pca_labs_color()
  ) +
  npgTheme()

}
