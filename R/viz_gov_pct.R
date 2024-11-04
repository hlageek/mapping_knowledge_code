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
viz_gov_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

  
applied_pis <- arrow::read_feather(isvavai_data$cep_investigators$path) |> 
filter(role_researcher == "G" & role_institution == "primary") |> 
left_join(
  # get data on providers for grants and match to PIs
  # keep only grants where funding is provided by government
arrow::read_feather(isvavai_data$cep_details$path) |> 
 filter(str_detect(provider, "^M")) |> 
 mutate(provider = "gov") |> 
 select(kod, provider)
) |> 
  # mark as TRUE every PI who has had a government funded project
summarise(
  gov_pi = any(provider == "gov"),
  .by = vedidk
) |> 
  # bind the result to all authors and remove duplicates
bind_rows(pca_projection |> select(vedidk)) |> 
filter(!duplicated(vedidk))  |> 
mutate(gov_pi = replace_na(gov_pi, FALSE))

pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(applied_pis) |> 
summarise(
    mean_gov_pct = mean(gov_pi, na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_gov_pct, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Government-funded PIs",
    x = pca_labs_x(),
    y = "Share of PIs",
    color = pca_labs_color()
  ) +
  npgTheme()

}
