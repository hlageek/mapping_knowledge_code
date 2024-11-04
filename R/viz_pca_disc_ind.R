#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pca_disc_ind
#' @return
#' @author hlageek
#' @export
viz_pca_disc_ind <- function(pca_disc, pca_disc_ind) {
  eigs <- pca_disc$sdev^2

  helper <- tidytable::tidytable(
    sd = sqrt(eigs),
    proportion = eigs / sum(eigs),
    cumulative = cumsum(eigs) / sum(eigs)
  )

  #  helper |> ggplot(aes(as.factor(cumulative), proportion)) + geom_col()

  xmin <- min(pca_disc_ind[["PC1"]])
  xmax <- max(pca_disc_ind[["PC1"]])
  ymin <- min(pca_disc_ind[["PC2"]])
  ymax <- max(pca_disc_ind[["PC2"]])
  ymin3 <- min(pca_disc_ind[["PC3"]])
  ymax3 <- max(pca_disc_ind[["PC3"]])

  var_plot1_2 <- factoextra::fviz_pca_var(pca_disc,
    axes = c(1, 2),
    geom = c("point", "text"),
    labelsize = 4,
    repel = TRUE
  ) +
    ggplot2::labs(
      title = NULL,
      x = paste0("PC1 (", round(helper$proportion[1] * 100, 2), "%)"),
      y = paste0("PC2 (", round(helper$proportion[2] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), color = "black", fill = NA)


  ind_plot1_2 <- ggplot2::ggplot(
    tidytable::as_tidytable(pca_disc_ind) |>
      tidytable::mutate(topic = stringr::word(topic_desc, 1, 1)),
    ggplot2::aes(PC1, PC2, label = topic)
  ) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    ggplot2::geom_point(color = "lightgrey", alpha = 0.5) +
    ggplot2::geom_text(check_overlap = TRUE, size = 3) +
    ggplot2::labs(
      title = NULL,
      x = paste0("PC1 (", round(helper$proportion[1] * 100, 2), "%)"),
      y = paste0("PC2 (", round(helper$proportion[2] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = xmin - 1, xmax = xmax + 1, ymin = ymin - 1, ymax = ymax + 1), color = "black", fill = NA) +
    ggplot2::theme_minimal()


  var_plot2_3 <- factoextra::fviz_pca_var(pca_disc,
    axes = c(2, 3),
    geom = c("point", "text"),
    labelsize = 4,
    repel = TRUE
  ) +
    ggplot2::labs(
      title = NULL,
      x = paste0("PC2 (", round(helper$proportion[2] * 100, 2), "%)"),
      y = paste0("PC3 (", round(helper$proportion[3] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), color = "black", fill = NA)


  ind_plot2_3 <- ggplot2::ggplot(
    tidytable::as_tidytable(pca_disc_ind) |>
      tidytable::mutate(topic = stringr::word(topic_desc, 1, 1)),
    ggplot2::aes(PC2, PC3, label = topic)
  ) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    ggplot2::geom_point(color = "lightgrey", alpha = 0.5) +
    ggplot2::geom_text(check_overlap = TRUE, size = 3) +
    ggplot2::labs(
      title = NULL,
      x = paste0("PC2 (", round(helper$proportion[2] * 100, 2), "%)"),
      y = paste0("PC3 (", round(helper$proportion[3] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = ymin - 1, xmax = ymax + 1, ymin = ymin3 - 1, ymax = ymax3 + 1), color = "black", fill = NA) +
    ggplot2::theme_minimal()

  var_plot3_4 <- factoextra::fviz_pca_var(pca_disc,
    axes = c(3, 4),
    geom = c("point", "text"),
    labelsize = 4,
    repel = TRUE
  ) +
    ggplot2::labs(
      title = "Contributions of disciplines in the topic space",
      x = paste0("PC3 (", round(helper$proportion[3] * 100, 2), "%)"),
      y = paste0("PC4 (", round(helper$proportion[4] * 100, 2), "%)")
    )

  ind_plot3_4 <- ggplot2::ggplot(
    tidytable::as_tidytable(pca_disc_ind) |>
      tidytable::mutate(topic = stringr::word(topic_desc, 1, 1)),
    ggplot2::aes(PC3, PC4, label = topic)
  ) +
    ggplot2::geom_point(color = "lightgrey") +
    ggplot2::geom_text(check_overlap = TRUE, size = 3) +
    ggplot2::labs(
      title = "Topic space structured by disciplines",
      x = paste0("PC3 (", round(helper$proportion[3] * 100, 2), "%)"),
      y = paste0("PC4 (", round(helper$proportion[4] * 100, 2), "%)")
    ) +
    ggplot2::theme_minimal()

  var_plot4_5 <- factoextra::fviz_pca_var(pca_disc,
    axes = c(4, 5),
    geom = c("point", "text"),
    labelsize = 4,
    repel = TRUE
  ) +
    ggplot2::labs(
      title = "Contributions of disciplines in the topic space",
      x = paste0("PC4 (", round(helper$proportion[4] * 100, 2), "%)"),
      y = paste0("PC5 (", round(helper$proportion[5] * 100, 2), "%)")
    )

  ind_plot4_5 <- ggplot2::ggplot(
    tidytable::as_tidytable(pca_disc_ind) |>
      tidytable::mutate(topic = stringr::word(topic_desc, 1, 1)),
    ggplot2::aes(PC4, PC5, label = topic)
  ) +
    ggplot2::geom_point(color = "lightgrey") +
    ggplot2::geom_text(check_overlap = TRUE, size = 3) +
    ggplot2::labs(
      title = "Topic space structured by disciplines",
      x = paste0("PC4 (", round(helper$proportion[4] * 100, 2), "%)"),
      y = paste0("PC5 (", round(helper$proportion[5] * 100, 2), "%)")
    ) +
    ggplot2::theme_minimal()

  list(
    var_plot1_2 = var_plot1_2,
    ind_plot1_2 = ind_plot1_2,
    var_plot2_3 = var_plot2_3,
    ind_plot2_3 = ind_plot2_3,
    var_plot3_4 = var_plot3_4,
    ind_plot3_4 = ind_plot3_4,
    var_plot4_5 = var_plot4_5,
    ind_plot4_5 = ind_plot4_5
  )
}
