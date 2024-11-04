#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param disc_top_profiles_clr
#' @return
#' @author hlageek
#' @export
run_disc_clustering <- function(disc_top_profiles_clr) {

  rownames(disc_top_profiles_clr) <- abbrev_discs(rownames(disc_top_profiles_clr))
  dist_m <- dist(disc_top_profiles_clr)
  hclust <- hclust(dist_m, method = "ward.D2")

}
