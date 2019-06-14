# this file reads in the simulation results and creates tables and figures for the paper.
setwd("/Users/alex/Dropbox/blups/code/rre_sims/tuning_sim_1")
library(magrittr)
library(dplyr)
library(ggplot2) 
library(latex2exp)
library(gridExtra)
library(grid)

thesis_method_comparison <- function(sim_data,
                                     output_name = NA, out_height = 6, out_width = 10) {
  sim_data %<>% dplyr::rename(Method = method)
  gg <- ggplot(sim_data, aes(x = alpha_delta_r, 
                             y = ccc_hat, 
                             col = Method, fill = Method,
                             label = r)) +
    geom_boxplot(outlier.colour = NULL, alpha = 0.6) +
    labs(y = TeX("$\\hat{C}$")) +
    geom_hline(yintercept = unique(sim_data$ccc)) +
    facet_grid(.~eta, scales = "free", switch = "x", space = "free_x", 
               labeller = labeller(eta = (function(x) paste0("eta = ",x)))) + 
    
    scale_fill_viridis_d(option = "viridis", begin = 0.15, end = 0.9) + 
    scale_color_viridis_d(option = "viridis", begin = 0.15, end = 0.9) +    
    scale_x_discrete(labels = paste0("r = ",rep(c(6,10,14),times = 2))) +
    theme_bw() +  
    theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
          axis.title.x=element_blank(),
          axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          #strip.placement = "outside",
          legend.position="bottom") 
  
  if (!is.na(output_name)) {
    ggsave(filename = output_name, plot = gg,
           height = out_height, width = out_width)
  } else {
    gg
  }
}

result <- readr::read_csv("./tuning_sim_1.csv")

thesis_method_comparison(result, output_name = "./method_sim_1.pdf")

# Information contained in the table for the primary simulation:
result %>%
  dplyr::group_by(method, eta, r) %>%
  dplyr::summarize(rmse = sqrt(mean(absolute_error^2))) %>%
  dplyr::arrange(eta, r, method) %>%
  print(n = 50)













