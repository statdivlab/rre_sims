# Figure and table creation for the expanded data analysis.

load("data_analysis_expanded_results.RData")

sample_outputs$sample_1 %>% lapply(., (function(x) c(x$ccc, x$selected_lambda, x$alpha, x$delta))) %>%
  do.call(rbind, .) %>%
  'colnames<-'(., c("ccc", "selected_lambda", "alpha", "delta")) %>%
  as.data.frame(.) %>%
  tibble::add_column(method = 0:4)

one_sample <- function(sample) {
  lapply(sample, (function(x) c(x$ccc, x$selected_lambda, x$alpha, x$delta))) %>%
    do.call(rbind, .) %>%
    'colnames<-'(., c("ccc", "selected_lambda", "alpha", "delta")) %>%
    as.data.frame(.) %>%
    tibble::add_column(method = 0:4)
}

all_samples <- function(sample_outputs) {
  lapply(sample_outputs, one_sample) %>% 
    do.call(rbind, .) %>%
    tibble::add_column(year = rep(2009:2011, each = 5)) %>%
    'rownames<-'(., NULL) %>%
    dplyr::select(year, method, ccc, selected_lambda, alpha, delta)
}


plot_fn <- function(output_df, 
                    output_name = NA,
                    output_height = 4,
                    output_width = 7) {
  # match method names to simulations:
  output_df %<>% dplyr::mutate(method_display = dplyr::case_when(
    method == 0 ~ "[0] MLE", # method 00
    method == 1 ~ "[1] Min Var", # method 01
    method == 2 ~ "[2] CV likelihood", # method 02
    method == 3 ~ "[3] GOF", # method 03
    method == 4 ~ "[4] CV GOF", # method 04 
    TRUE ~ "error")) %>%
    dplyr::mutate(method_display = factor(method_display))
  
  if (any(output_df$method_display == "error")) stop("Coding not recognized")
  
  pd <- position_dodge(0.1)
  gg <- ggplot(output_df, aes(x = year, y = ccc, 
                              group = method_display, 
                              color = method_display)) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    labs(y = latex2exp::TeX("$\\hat{C}$"), x = "Year") + 
    scale_color_viridis_d("Method", option = "viridis", begin = 0.15, end = 0.9) +    
    scale_x_continuous(breaks = 2009:2011, minor_breaks = 2009:2011) +
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


all_samples(sample_outputs) %>% plot_fn(.)

