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
viz_gender_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

  
pub_fem <-  arrow::read_feather(isvavai_data$riv_authors$path) |> 
select(vedidk, gender) |> 
distinct()

pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(pub_fem) |> 
summarise(
    mean_fem = mean(gender == "female", na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_fem, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Female authors",
    x = pca_labs_x(),
    y = "Share of female authors",
    color = pca_labs_color()
  ) +
  npgTheme()

}
