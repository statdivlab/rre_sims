# Figure and table creation for the expanded data analysis.

load("data_analysis_expanded_results.RData")

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
  
  pd <- position_dodge(0)
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

sample_outputs %>% all_samples %>% str

# compact table output:
table_to_console_1 <- function(outs) {
  outs %<>% all_samples  
  outs %<>% dplyr::mutate(method = dplyr::case_when(
    method == 0 ~ "{[0]} Unpenalized MLE", # method 00
    method == 1 ~ "{[1]} Minimum Variance", # method 01
    method == 2 ~ "{[2]} CV Likelihood", # method 02
    method == 3 ~ "{[3]} Goodness of fit", # method 03
    method == 4 ~ "{[4]} CV G.O.F.", # method 04 
    TRUE ~ "error")) %>%
    dplyr::mutate(method = factor(method))
  
  if (any(outs$method_display == "error")) stop("Coding not recognized")
  
  outs %<>% dplyr::mutate(ccc = formatC(ccc, format="d", big.mark=","),
                         selected_lambda = ifelse(is.na(selected_lambda), 
                                                  "\\textemdash", 
                                                  as.character(selected_lambda)),
                         alpha = formatC(alpha, format = "f", digits = 5),
                         delta = formatC(delta, format = "f", digits = 5))
  # This is rearrangement for latex output:
  cols_1 <- outs[1:5,2:6]
  cols_2 <- outs[6:10,3:6]
  cols_3 <- outs[11:15,3:6]
  flat_table <- cbind(cols_1,cols_2,cols_3)
  
  ft_names <- c("$\\widehat{C}$", "$\\widetilde{\\lambda}$",
                "$\\widehat{\\alpha}$", "$\\widehat{\\delta}$")
  colnames(flat_table) <- c("Method", rep(ft_names, times = 3))
  flat_table %>% xtable::xtable(.) %>% print(., 
                                          sanitize.text.function = identity,
                                          sanitize.rownames.function = identity,
                                          santitize.colnames.function = identity,
                                          include.rownames = F,
                                          booktabs = T)
  # after this we manually insert multicolumn row for years: 2009, 2010 and 2011.
}

# condensed table output:
table_to_console_2 <- function(outs) {
  # Function starts the same as table_to_console_1:
  outs %<>% all_samples  
  outs %<>% dplyr::mutate(method = dplyr::case_when(
    method == 0 ~ "{[0]} Unpenalized MLE", # method 00
    method == 1 ~ "{[1]} Minimum Variance", # method 01
    method == 2 ~ "{[2]} CV Likelihood", # method 02
    method == 3 ~ "{[3]} Goodness of fit", # method 03
    method == 4 ~ "{[4]} CV G.O.F.", # method 04 
    TRUE ~ "error")) %>%
    dplyr::mutate(method = factor(method))
  
  if (any(outs$method_display == "error")) stop("Coding not recognized")
  
  outs %<>% dplyr::mutate(ccc = formatC(ccc, format="d", big.mark=","),
                          selected_lambda = ifelse(is.na(selected_lambda), 
                                                   "\\textemdash", 
                                                   as.character(selected_lambda)),
                          alpha = formatC(alpha, format = "f", digits = 5),
                          delta = formatC(delta, format = "f", digits = 5))
  # Output formatting from here is new: 
  cols_1 <- outs[1:5,1:6]
  cols_2 <- outs[6:10,1:6]
  cols_3 <- outs[11:15,1:6]
  
 
  flat_table <- rbind(cols_1,cols_2,cols_3)
  
  year_vec <- c("2009", "2010", "2011", "\\multirow{5}{*}")
  
  flat_table[,1] <- year_vec[c(1,rep(4,4),2,rep(4,4),3,rep(4,4))]
  
  ft_names <- c("Year", "Method", "$\\widehat{C}$", "$\\widetilde{\\lambda}$",
                "$\\widehat{\\alpha}$", "$\\widehat{\\delta}$")
  colnames(flat_table) <- ft_names
  flat_table %>% xtable::xtable(.) %>% print(., 
                                             sanitize.text.function = identity,
                                             sanitize.rownames.function = identity,
                                             santitize.colnames.function = identity,
                                             include.rownames = F,
                                             booktabs = T)
  # after this we manually insert multicolumn row for years: 2009, 2010 and 2011.
}

sample_outputs %>% table_to_console_1
sample_outputs %>% table_to_console_2

sample_outputs %>% all_samples %>% plot_fn(.)
sample_outputs %>% all_samples %>% plot_fn(., "../rre_manuscript/images/data_analysis_expanded.pdf")


