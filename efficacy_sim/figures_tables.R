setwd("/Users/alex/Dropbox/blups/code/rre_sims/efficacy_sim/")

library(magrittr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(gridExtra)
library(grid)
library(xtable)

create_results_table <- function(result) {
  
  lam_zero_table <- result %>%
    filter(lambda == 0) %>%
    group_by(alpha, delta, r) %>%
    summarize(rmse_lambda_zero = sqrt(mean((absolute_error)^2))) %>%
    arrange(alpha, delta, r) %>%
    '['(c(10:12,1:3,7:9,4:6),) # my preferred order
  
  optimal_table <- result %>% 
    group_by(alpha, delta, r, lambda) %>%
    summarize(rmse_lambda_opt = sqrt(mean((absolute_error)^2))) %>%
    group_by(alpha, delta, r) %>%
    arrange(rmse_lambda_opt) %>%
    filter(1:n() == 1) %>%
    ungroup() %>%
    arrange(alpha, delta, r) %>%
    rename(lambda_opt = lambda) %>%
    '['(c(10:12,1:3,7:9,4:6),)
  
  output_table <- dplyr::left_join(lam_zero_table, optimal_table) %>% ungroup
  
  param_format <- function(column) {
    column %>%
      format(., scientific = T) %>%
      as.character 
  }
  
  output_table$alpha <- param_format(output_table$alpha)
  output_table$delta <- param_format(output_table$delta)
  
  output_table$eta <- with(output_table,
                           paste0("(", alpha, ", ",  delta, ")"))
  print(output_table$eta)
  output_table %<>%
    dplyr::select(-alpha, -delta) %>%
    dplyr::select(eta, everything())
  
  colnames(output_table) <- c("$\\eta$", "$r$",
                              "RMSE ($\\lambda = 0$)",
                              "$\\lambda_{opt}$",
                              "RMSE ($\\lambda = \\lambda_{opt}$)")
  output_table %>%
    xtable(.) %>% 
    print(.,sanitize.colnames.function=identity, 
          sanitize.text.function = identity, include.rownames = F)
  
}

thesis_fixed_lambda <- function(df, alpha, delta,
                                output_file = NA,
                                output_height = 6,
                                output_width = 10) {
  total_rows <- nrow(df)
  subset_ind <- (df$alpha == alpha & df$delta == delta)
  df %<>% dplyr::filter(subset_ind)
  print(paste0("Plotting with ", nrow(df), 
               " observations selected (", 100*nrow(df)/total_rows, "%)"))
  df$r <- factor(df$r, levels = sort(unique(df$r)))
  
  alpha_exp <- log10(alpha)
  delta_exp <- log10(delta)
  
  ccc_plot <- df %>% filter((df$lambda %% 5 == 0)) %>% 
    dplyr::mutate(lambda_f = factor(.$lambda)) %>%
    ggplot(.) +
    geom_boxplot(aes(x = lambda_f, y = ccc_hat, fill = r, color = r)) +
    geom_hline(yintercept = 1000) +
    #annotate("text", label = "C = 1000", x = 20, y = 1150, size = 3, colour = "black") + 
    labs(title = TeX(paste0("$\\eta = (10^{",alpha_exp,"}, ",
                            "10^{",delta_exp,"})")),
         x = TeX("$\\lambda$"),
         y = TeX("$\\widehat{C}$")) + 
    scale_fill_viridis_d(option = "inferno", begin = 0.25, end = 0.75) + 
    scale_color_viridis_d(option = "inferno", begin = 0.25, end = 0.75) +     
    #  coord_cartesian(xlim = c(0, 120)) +
    theme_bw() +
    theme(legend.position="none", 
          axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = .5),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0),  
          axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0),
          axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5))
  rmse_plot <-  df %>% filter((df$lambda %% 5 == 0)) %>% 
    dplyr::mutate(lambda_f = factor(.$lambda)) %>%
    group_by(r, lambda_f) %>%
    summarize(rmse = sqrt(mean((absolute_error)^2))) %>%
    ggplot(., aes(x = lambda_f, y = rmse, group = r, color = r)) +
    geom_line() +
    labs(x = TeX("$\\lambda$"),
         y = TeX("$RMSE(\\widehat{C})$")) +    
    scale_fill_viridis_d(option = "inferno", begin = 0.25, end = 0.75) + 
    scale_color_viridis_d(option = "inferno", begin = 0.25, end = 0.75) + 
    guides(colour = guide_legend(override.aes = list(size=2, stroke=2))) + 
    theme_bw() +
    theme(legend.position="bottom",
          axis.text.x = element_text( size = 12, angle = 0, hjust = .5, vjust = .5),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
          axis.title.x = element_text( size = 12, angle = 0, hjust = .5, vjust = 0),
          axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5))
  #plot.margin = margin(l = 0.12, unit = "in"))
  
  plot_grid <- grid.arrange(
    grobs = list(ccc_plot, rmse_plot),
    layout_matrix = matrix(c(1,1,2),nrow = 3),
    nrow = 1)
  if (!is.na(output_file)) {
    ggsave(filename = output_file, plot = plot_grid,
           height = output_height, width = output_width)
  } else {
    plot_grid
  }
}


result <- readr::read_csv("./efficacy_sims.csv")


thesis_fixed_lambda(result, alpha = 0.1, delta = 0.1,
                    output_height = 5, output_width = 10,
                    output_file = "../../../thesis/images/fixed_lambda_eta1.pdf")
thesis_fixed_lambda(result, alpha = 0.01, delta = 0.00001,
                    output_height = 5, output_width = 10,
                    output_file = "../../../thesis/images/fixed_lambda_eta2.pdf")

# Results table for efficacy simulation:
create_results_table(result)


