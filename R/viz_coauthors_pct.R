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
viz_coauthors_pct <- function(pca_projection, isvavai_data, min_pubs = 2) {

authors_pubs <- arrow::read_feather(isvavai_data$authors_by_pubs$path) |> 
select(vedidk, id_unique) |> 
distinct()
    
no_coauthors <- arrow::read_feather(isvavai_data$riv_details$path) |> 
select(id_unique, no_authors) |> 
distinct() |> 
na.omit() |> 
filter(no_authors < 100) |> 
mutate(no_authors = no_authors - 1)

coauthors <- authors_pubs |> 
left_join(no_coauthors) |> 
summarise(no_coauthors = mean(no_authors, na.rm = TRUE), .by = vedidk)   




pca_projection |> 
filter(pubs_total > min_pubs) |> 
reshape_pca_projection(coauthors) |> 
summarise(
    mean_coauthors = mean(no_coauthors, na.rm = TRUE),
    .by = c(axis, percentile)
  ) |> 
  ggplot(aes(percentile, mean_coauthors, color = axis)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = npgPalette(),
    labels = pca_labels()
   ) +
  labs(
    title = "Co-authors",
    x = pca_labs_x(),
    y = "Number of co-authors",
    color = pca_labs_color()
  ) +
  npgTheme()

}
