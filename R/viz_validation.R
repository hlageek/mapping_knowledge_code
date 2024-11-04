#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_articles_viz
#' @param pca_gender_viz
#' @return
#' @author hlageek
#' @export
viz_validation <- function(
  pca_articles_viz,
  pca_gender_viz,
  pca_pi_viz,
  pca_cas_viz,
  pca_hospital_viz,
  pca_art_viz,
  pca_coauthors_viz,
  pca_applied_viz,
  pca_gov_viz
  ) {

  require(patchwork)

  panel <- pca_articles_viz +
  pca_gender_viz +
  pca_coauthors_viz +
  pca_cas_viz +
  pca_hospital_viz +
  pca_art_viz +
  pca_pi_viz +
  pca_applied_viz +
  pca_gov_viz +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 16),
  plot.background = element_rect(colour = NA)
  )

return(panel)

}
