#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param isvavai_data_files
#' @param topic_model_files
#' @return
#' @author hlageek
#' @export
parse_data_files <- function(data_files) {
  paths <- as.list(stringr::str_subset(data_files, "arrow"))
  data_names <- purrr::map_chr(paths, ~basename(tools::file_path_sans_ext(.x)))
  named_paths <- stats::setNames(paths, data_names)
  purrr::map(named_paths, ~list(path = .x))
}
