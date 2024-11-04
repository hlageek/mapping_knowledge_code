#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_matrix
#' @return
#' @author hlageek
#' @export
name_topics <- function(topic_matrix) {

  arrow::read_feather(topic_matrix) |> 
    mutate(across(starts_with("topic"), ~ na_if(.x, 0))) |>
      tidytable::pivot_longer(starts_with("topic"), names_to = "topic", values_drop_na = T) |>
      group_by(topic) |>
      arrange(topic, desc(value)) |>
      summarize(topic_desc = paste(term, collapse = " "))

}
