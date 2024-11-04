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
viz_hospital_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

  
pub_affiliation <- arrow::read_feather(isvavai_data$authors_by_pubs$path) |> 
select(vedidk, id_unique, org_name) |> 
distinct() |> 
na.omit() |> 
mutate(cas = str_detect(org_name, "[Nn]emocnice"))  |> 
summarise(cas = mean(cas, na.rm = TRUE), .by = vedidk) 

pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(pub_affiliation) |> 
summarise(
    mean_fem = mean(cas, na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_fem, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Hospitals",
    x = pca_labs_x(),
    y = "Share of hospital affiliations",
    color = pca_labs_color()
  ) +
  npgTheme()

}
