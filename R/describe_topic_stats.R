#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_model_files
#' @return
#' @author hlageek
#' @export
describe_topic_stats <- function(topic_model_files) {
  top_mod_names <- stringr::str_subset(basename(topic_model_files), "topics")
  doc_mod_names <- stringr::str_subset(basename(topic_model_files), "documents")

  df_big <- tidytable::map2_df(top_mod_names, doc_mod_names, .f = function(x, y) {
    model_top <- arrow::read_feather(stringr::str_subset(topic_model_files, x)) |> 
    tidytable::as_tidytable()
    model_doc <- arrow::read_feather(stringr::str_subset(topic_model_files, y)) |> nrow()

    topics_long <- model_top |>
      tidytable::select(term, starts_with("topic")) |>
      tidytable::pivot_longer(-term, names_to = "topic") |>
      tidytable::filter(value > 0)

    topics_stats <- topics_long |>
      tidytable::summarise(n_terms = n(), .by = "topic") |>
      tidytable::summarise(
        mean_n_terms = mean(n_terms),
        sd_n_terms = sd(n_terms)
      )

    hierarchy <- model_top$level |> unique()

    df <- tidytable(
      `Level` = hierarchy,
      `Documents` = model_doc,
      `Terms` = nrow(model_top),
      `Topics` = model_top |>
        select(starts_with("topic")) |>
        ncol(),
      `Terms per topic (mean)` = round(topics_stats$mean_n_terms, 2),
      `Terms per topic (sd)` = round(topics_stats$sd_n_terms, 2)
    )
  })

  table <- df_big |>
    gt::gt() |>
    gt::cols_hide(c(Documents, Terms)) |> 
    gt::opt_table_font("CMU Serif")
}
