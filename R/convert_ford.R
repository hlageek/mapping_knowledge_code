#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param isvavai_data
#' @return
#' @author hlageek
#' @export
convert_ford <- function(isvavai_data) {

  arrow::read_feather(isvavai_data$riv_details$path) |> 
              select(ford, field) |> 
              count(ford, field) |> 
              na.omit() |> 
              slice_max(n, .by = ford) |> 
              select(freq_ford = ford, freq_field = field)  
}
