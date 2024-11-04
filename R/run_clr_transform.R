#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param disc_top_profiles
#' @return
#' @author hlageek
#' @export
run_clr_transform <- function(disc_top_profiles) {

  column_name <- colnames(disc_top_profiles[,1])
  column_values <- disc_top_profiles[,1][[1]]
  
  mat <- as.matrix(disc_top_profiles[,-1])
  rownames(mat) <- column_values
  clr_mat <- compositions::clr(mat)
  compositions::scale(clr_mat)
  

}
