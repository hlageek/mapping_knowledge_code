#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_disc_ind
#' @return
#' @author hlageek
#' @export
interpret_coordinates_tbl <- function(pca_disc_ind, n_top = 10) {

require(gt)
table_data <- pca_disc_ind |> 
select(topic_desc, PC1, PC2, PC3) |> 
pivot_longer(starts_with("PC"), names_to = "Axis", values_to = "Score") |> 
nest(.by = Axis) |> 
mutate(data = map(data, .f = function(x) {
  x |> 
mutate(topic_desc = stringr::str_extract(topic_desc, "^([\\w]+\\s){0,5}[\\w]+"))  |> 
mutate(PC_pole = ifelse(Score < 0, "min", "max"),
       PC_abs = abs(Score)) |> 
slice_max(PC_abs, n = n_top, .by = PC_pole) |> 
arrange(desc(PC_abs), .by = PC_pole)  |> 
mutate(rank = row_number(),  .by = PC_pole) |> 
pivot_wider(names_from = PC_pole, values_from = c(topic_desc, PC_abs, Score), id_cols = rank)
})) |> 
unnest()  |> 
mutate(axis_helper = Axis) |> 
nest(.by = axis_helper)

map(table_data$data, .f = function(x) {
  x |> 
  select(Axis, topic_desc_min, topic_desc_max) |> 
  gt::gt() |> 
      grand_summary_rows(
    columns = c(topic_desc_min, topic_desc_max),
     fns = list(
      label = md("**Interpretation**")) ~ tidytable::case_when(
        stringr::str_detect(.x, "political") ~ "Culture", 
        stringr::str_detect(.x, "cell") ~ "Nature", 
        stringr::str_detect(.x, "prevalence") ~ "Life", 
        stringr::str_detect(.x, "numerical") ~ "Non-life", 
        stringr::str_detect(.x, "pottery") ~ "Materials", 
        stringr::str_detect(.x, "randomly") ~ "Methods"
        )) |> 
     cols_hide(Axis)  |> 
     cols_label(
    topic_desc_min = md("_Negative pole_"),
    topic_desc_max = md("_Positive pole_")
  ) |> 
   opt_table_font("CMU Serif") |> 
   tab_options(table.width = pct(100))
})



}
