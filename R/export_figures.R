export_figures <- function(figure, path, filename = NULL, width = NA, height = NA) {
  if (is.null(filename)) {
    filename <- deparse(substitute(figure))
  }

  file_path <- paste0(path, .Platform$file.sep, filename, ".png")

  if (!dir.exists(path)) dir.create(path)

  if (path != "") {
    ggplot2::ggsave(
      filename = file_path,
      plot = figure,
      width = width,
      height = height,
      units = "cm",
      device = "png",
      bg = "white"
    )
  } else {
    NULL
  }
  file_path
}
