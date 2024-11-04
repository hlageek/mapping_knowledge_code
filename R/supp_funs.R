

viz_disc_top <- function(pca_disc, pca_disc_ind, pcx_chr, pcx, pcy_chr, pcy, helper) {
  xmin <- min(pca_disc_ind[[pcx_chr]])
  xmax <- max(pca_disc_ind[[pcx_chr]])
  ymin <- min(pca_disc_ind[[pcy_chr]])
  ymax <- max(pca_disc_ind[[pcy_chr]])


  var_plot <- factoextra::fviz_pca_var(pca_disc,
    axes = c(pcx, pcy),
    geom = c("point", "text"),
    labelsize = 2,
    repel = TRUE
  ) +
    ggplot2::labs(
      title = NULL,
      x = paste0(pcx_chr, " (", round(helper$proportion[pcx] * 100, 2), "%)"),
      y = paste0(pcy_chr, " (", round(helper$proportion[pcy] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), color = "black", fill = NA)

  pcx_sym <- rlang::sym(pcx_chr)
  pcy_sym <- rlang::sym(pcy_chr)


  ind_plot <- ggplot2::ggplot(
    tidytable::as_tidytable(pca_disc_ind) |>
      tidytable::mutate(topic = stringr::word(topic_desc, 1, 1)),
    ggplot2::aes(!!pcx_sym, !!pcy_sym, label = topic)
  ) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    ggplot2::geom_point(color = "lightgrey", alpha = 0.5) +
    ggplot2::geom_text(check_overlap = TRUE, size = 2) +
    ggplot2::labs(
      title = NULL,
      x = paste0(pcx_chr, " (", round(helper$proportion[pcx] * 100, 2), "%)"),
      y = paste0(pcy_chr, " (", round(helper$proportion[pcy] * 100, 2), "%)")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_rect(aes(xmin = xmin - 1, xmax = xmax + 1, ymin = ymin - 1, ymax = ymax + 1), color = "black", fill = NA) +
    ggplot2::theme_minimal()

list(
  disciplines = var_plot,
  topics = ind_plot
)
}

defining_topics <- function(pca_disc_ind, pc, n) { 
  pc_sym <- rlang::sym(pc)
  
  defining_topics <- pca_disc_ind |> 
    select(topic_desc, !!pc_sym) |> 
    mutate(PC_pole = ifelse(!!pc_sym <= 0, "min", "max")) |> 
    mutate(PC_abs = abs(!!pc_sym)) |> 
    slice_max(PC_abs, n = n, by = PC_pole) |> 
    mutate(topic_desc = stringr::str_extract(topic_desc, "^([\\w]+\\s){0,5}[\\w]+")) |> 
    arrange(desc(PC_abs))
  
  list(
    min = defining_topics |> 
      filter(PC_pole == "min") |> 
      pull(topic_desc),
    max = defining_topics |> 
      filter(PC_pole == "max") |> 
      pull(topic_desc)
  )
}