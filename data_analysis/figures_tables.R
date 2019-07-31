library(magrittr)
library(rre)
library(ggplot2)

get_n <- function(list_of_samples) {
  get_n_one_sample <- function(samp) samp[[1]] %>% length
  lapply(list_of_samples, get_n_one_sample) %>% unlist
}

# Temporary while running C grid sensitivity analysis:
get_cc <- function(list_of_results) {
  one_sample <- function(samp) samp[[1]] %>% rre::get_cc_max(.)
  lapply(list_of_results, one_sample) %>% unlist
}

get_estimate <- function(result) {
  best <- result %>% '[['(.,"best") 
  if ("ccc" %in% names(best)) {
    best[["ccc"]]
  } else if ("ccc_hat" %in% names(best)) {
    best[["ccc_hat"]]
  } else {
    stop("Estimate not found for this method")
  }
}

process_sample <- function(list_of_outputs) {
  list_of_outputs %<>% '['(., 2:6) # the first list item is just the data, so 2-6 are the 5 methods
  lapply(list_of_outputs, get_estimate) %>% unlist
}

process_sample_list <- function(list_of_sample_outs, 
                                sample_names = NA,
                                latex_format = T) {
  table_output <- lapply(list_of_sample_outs, process_sample) %>%
    do.call(cbind,.)
  if (any(!is.na(sample_names))) {
    colnames(table_output) <- sample_names
  }
  
  if (latex_format) {
    r <- get_n(lakes_results)
    table_output %<>% rbind(.,r)
    rownames(table_output) <- c( paste0("$\\widehat{C}_{[", 0:4, "]}$"),  "$r$")
    table_output %>% xtable::xtable(digits = 0) %>% print(
      sanitize.text.function = identity,
      sanitize.rownames.function = identity,
      santitize.colnames.function = identity
    )
  } else { # console output
    ceiling(table_output)
  }
}

plot_fn <- function(output_mat, 
                    output_name = NA,
                    output_height = 4,
                    output_width = 7) {
  # match method names to simulations:
  rownames(output_mat) <- c("[0] MLE", # method 00
                    "[1] Min Var", # method 01
                    "[2] CV likelihood", # method 02
                    "[3] GOF", # method 03
                    "[4] CV GOF") # method 04
  
  df <- output_mat %>%
    as.data.frame %>%
    tibble::rownames_to_column(.) %>%
    tidyr::gather(., key = "year", value = "estimate", -rowname) %>%
    dplyr::rename(method = rowname)
  
  print(df)
  
  pd <- position_dodge(0.1)
  gg <- ggplot(df, aes(x = year, y = estimate, group = method, color = method)) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    labs(y = latex2exp::TeX("$\\hat{C}$"), x = "Year") + 
    scale_color_viridis_d(option = "viridis", begin = 0.15, end = 0.7125) +
    theme_bw() +
    theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
          axis.title.x=element_blank(),
          axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          #strip.placement = "outside",
          legend.position="bottom") 

  
  if (!is.na(output_name)) {
    ggsave(filename = output_name, plot = gg,
           height = output_height, width = output_width)
  } else {
    gg
  }
}




setwd("/Users/alex/Dropbox/classGrad/IS_Wil/rre_sims/data_analysis/")

load("data_analysis_results.RData")
#load("data_analysis_results_big_C.RData")

# latex table outputs:
process_sample_list(lakes_results, 
                    sample_names = 2009:2011,
                    latex_format = T)

#process_sample_list(lakes_results, 
#                    sample_names = 2009:2011,
#                    latex_format = T)

process_sample_list(lakes_results, 
                    sample_names = 2009:2011,
                    latex_format = F) %>%
  plot_fn(.) #"../rre_manuscript/images/data_analysis.pdf")

