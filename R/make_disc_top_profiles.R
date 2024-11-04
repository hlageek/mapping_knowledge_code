#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topmat_clean
#' @param disc_topmat_index
#' @return
#' @author hlageek
#' @export
make_disc_top_profiles <- function(doc_topic_path, isvavai_data) {

  
df <- as_tidytable(arrow::read_feather(doc_topic_path)) |> 
rename("id_unique" = "doc")

disc_topmat_index <- as_tidytable(arrow::read_feather(isvavai_data$riv_details$path))

main <- disc_topmat_index |> 
select(id_unique, ford) |> 
filter(!is.na(ford)) |> 
mutate(id_unique = as.character(id_unique)) |> 
summarise(n = n(), .by = c(id_unique, ford)) |> 
filter(n == max(n), .by = id_unique) |> 
distinct(id_unique, .keep_all = TRUE) |> 
select(-n) 

intermediate <- main |> 
inner_join(df, .by = "id_unique")

res <- intermediate  |> 
summarise(across(starts_with("topic"), mean), .by = "ford")

}

