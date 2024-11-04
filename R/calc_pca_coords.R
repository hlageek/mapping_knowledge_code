#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_disc
#' @param topic_names
#' @return
#' @author hlageek
#' @export
calc_pca_coords <- function(pca_disc, topic_names, varimax = FALSE) {

 if (varimax) {
  coords_df <- tidytable::as_tidytable(
      pca_disc$x %*% varimax(pca_disc$rotation)$rotmat
      ) 
  colnames(coords_df)  <- colnames(pca_disc$rotation)
 } else {
    coords_df <- tidytable::as_tidytable(
      pca_disc$x 
      ) 
 }

    coords_df |> 
    tidytable::mutate(topic = rownames(pca_disc$x)) |> 
    tidytable::relocate(topic, .before = 1) |> 
    tidytable::left_join(topic_names)  |> 
    tidytable::relocate(topic_desc, .after = 1)



}
