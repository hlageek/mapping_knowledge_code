#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param disc_top_profiles
#' @return
#' @author hlageek
#' @export
run_pca <- function(disc_top_profiles_clr) {
mat_t <- t(disc_top_profiles_clr)

colnames(mat_t) <- abbrev_discs(rownames(disc_top_profiles_clr))
rownames(mat_t) <- colnames(disc_top_profiles_clr)

# scaling was already performed during clr transform
pca <- prcomp(mat_t, center = TRUE, scale = FALSE)

# To multiply the first PC axis by -1
pca$x[,1] <- -1 * pca$x[,1]
pca$rotation[,1] <- -1 * pca$rotation[,1]

pca

}
