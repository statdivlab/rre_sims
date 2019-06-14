#####################################################
### Pipeline once simulations complete on cluster ###
#####################################################

setwd("/Users/alex/Dropbox/blups/code/rre_sims/tuning_sim_1")
library(simulator); library(magrittr);
source("method_functions.R")
source("model_functions.R")
source("eval_functions.R")

load_and_process_subset <- function(subset_id) {
  str <- stringr::str_pad(subset_id, width = 2, side = "left", pad = "0")
  sim <- load_simulation(paste0("sim_subset_", str),
                         dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    evaluate(list(ccc, alpha, delta, r,
                  ccc_hat, absolute_error, alpha_hat, delta_hat,
                  selected_lambda)) %>%
    evals %>%
    as.data.frame
  sim$sim_subset <- rep(subset_id, times = nrow(sim))
  sim$draw <- sim$Draw %>%
    stringr::str_split(., "\\.") %>%
    lapply(., (function(x) x[[2]])) %>%
    unlist
  sim$id <- (as.numeric(sim$sim_subset)-1)*5+as.numeric(sim$draw)
  sim$id <- factor(sim$id)
  sim %<>% dplyr::rename(method = Method)
  
  sim %<>% dplyr::select(-Model, -Draw) %>%
    dplyr::arrange(delta, r) %>% 
    dplyr::mutate(eta = paste0("(",
                               formatC(alpha, format = "e", digits = 0),
                               ", ", 
                               formatC(delta, format = "e", digits = 0), 
                               ")")) %>%
    dplyr::mutate(alpha_delta_r = paste0("(", eta, ",", r, ")"))
  
  sim$method <- factor(sim$method,
                       levels = c("mle_no_pen", # method 00
                                  "min_sub_dist", # method 01
                                  "cv_likelihood", # method 02
                                  "gof", # method 03
                                  "cv_gof"), # method 04
                       labels = c("[0] MLE", # method 00
                                  "[1] Min Var", # method 01
                                  "[2] CV likelihood", # method 02
                                  "[3] GOF", # method 03
                                  "[4] CV GOF")) # method 04
  sim %<>% dplyr::arrange(desc(alpha),r,method)
  sim$alpha_delta_r <- factor(sim$alpha_delta_r,
                              levels = unique(sim$alpha_delta_r))
  return(sim)
}

load_and_process_all <- function() {
  list_of_sims <- lapply(1:20, load_and_process_subset)
  df <- data.table::rbindlist(list_of_sims)
  return(df)
}

result <- load_and_process_all() # takes approx 1 minute.
readr::write_csv(result, "./tuning_sim_1.csv")
rm(list = ls())
