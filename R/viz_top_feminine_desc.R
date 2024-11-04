viz_top_feminine_desc <- function(top_feminine_desc) {
    
    plot_data <- top_feminine_desc %>% bind_rows()
    
    global_mean <- plot_data %>%
        # must round, because the decimals are not represented equally
        # across the different target dataframes
        mutate(g_ratefemale = round(g_ratefemale,5)) %>% 
        pull(g_ratefemale) %>% unique()
    
     g1 <- plot_data %>% 
        mutate(level = as.factor(level)) %>% 
        ggplot(aes(w_ratefemale, fill = level,  color = level)) +
        geom_histogram(alpha = 0.5, position = 'identity') +
        geom_vline(aes(xintercept = 0.5, linetype = "Parity")) +
        geom_vline(aes(xintercept = global_mean, linetype = "Global mean")) +
        scale_linetype_manual(values = c(1,2)) +
        theme_minimal() +
         labs(x = "Topic-weighted rate of female authors",
              linetype = "")
    
    g2 <- plot_data %>% 
        mutate(level = as.factor(level)) %>% 
        ggplot(aes(w_ratefemale, fill = level, color = level)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = 0.5, linetype = 2) +
        geom_vline(xintercept = global_mean, linetype = 1) +
        theme_minimal() +
        labs(x = "Topic-weighted rate of female authors") +
        facet_wrap(~level) 
    

    
    g_patched <- patchwork::wrap_plots(g1, g2, 
                                       ncol = 1,
                                       guides = "collect")
    
    return(g_patched)
}