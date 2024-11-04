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
viz_articles_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

pub_types <- arrow::read_feather(isvavai_data$riv_details$path) |> 
select(id_unique, pub_type)

pub_authors <- arrow::read_feather(isvavai_data$authors_by_pubs$path) |> 
select(id_unique, vedidk) |> 
distinct()

share_articles <- pub_authors |> 
left_join(pub_types) |> 
group_by(vedidk, pub_type) |> 
count()  |> 
ungroup() |> 
pivot_wider(
  names_from = pub_type, 
  values_from = n, 
  values_fill = 0) |> 
mutate(pct_articles = J/(B+J), .by = vedidk) |> 
select(vedidk, pct_articles)

pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(share_articles) |> 
  summarise(
    mean_art = mean(pct_articles, na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_art, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Articles",
    x = pca_labs_x(),
    y = "Share of articles",
    color = pca_labs_color()
  ) +
  npgTheme()

}
