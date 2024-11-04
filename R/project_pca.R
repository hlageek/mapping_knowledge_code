#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_disc_ind
#' @param keep number of dimensions to keep
#' @return
#' @author hlageek
#' @export
project_pca <- function(pca_disc_ind, people, keep = 5) {

ppl_vars <-  people |> 
      tidytable::filter(!duplicated(vedidk)) |> 
      tidytable::select(any_of(c("vedidk", "freq_field", "freq_ford", "gender", "pubs_total", "id_helper"))) 

mat_pca <- pca_disc_ind |> 
tidytable::select(starts_with("PC"))  |> 
tidytable::select(tidyselect::all_of(c(1:keep))) |> 
as.matrix()

mat_people <- people  |> 
tidytable::filter(!duplicated(vedidk))|> 
tidytable::select(starts_with("topic")) |> 
as.matrix()

res <- mat_people %*% mat_pca

res_tbl <- tidytable::as_tidytable(res) 

final <-   tidytable::bind_cols(
      res_tbl, ppl_vars
    ) 

return(final)

}
