#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topmat_clean
#' @param topmat_index
#' @return
#' @author hlageek
#' @export
make_people_top_profiles <- function(document_topic_matrix, isvavai_data) {

df <- as_tidytable(arrow::read_feather(document_topic_matrix)) |> 
rename("id_unique" = "doc")

vedidk_topmat_index <- as_tidytable(arrow::read_feather(isvavai_data$authors_by_pubs$path))

main <- vedidk_topmat_index |> 
select(id_unique, vedidk) |> 
filter(!is.na(vedidk))  |> 
mutate(id_unique = as.character(id_unique)) |> 
filter(id_unique %in% df$id_unique)

intermediate <- main |> 
left_join(df, .by = "id_unique")

res <- intermediate  |> 
summarise(across(starts_with("topic"), mean), .by = "vedidk")
}
