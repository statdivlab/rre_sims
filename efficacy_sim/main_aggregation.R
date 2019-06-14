#####################################################
### Pipeline once simulations complete on cluster ###
#####################################################

setwd("/Users/alex/Dropbox/blups/code/rre_sims/efficacy_sim/")
library(simulator); library(magrittr);
source("method_functions.R")
source("model_functions.R")
source("eval_functions.R")

load_and_process_subset <- function(subset_id) {
  str <- stringr::str_pad(subset_id, width = 3, side = "left", pad = "0") 
  sim <- load_simulation(paste0("sim_subset_", str),
                         dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    evaluate(list(ccc, alpha, delta, r,
                  ccc_hat, absolute_error, alpha_hat, delta_hat,
                  selected_lambda)) %>% #chi_squared_gof)) %>%
    evals %>%
    as.data.frame
  # sloppy way of differentiating the sim repetitions done in each subset:
  sim$sim_subset <- rep(subset_id, times = nrow(sim))
  sim$draw <- sim$Draw %>% 
    stringr::str_split(., "\\.") %>% 
    lapply(., (function(x) x[[2]])) %>% 
    unlist
  sim$id <- (as.numeric(sim$sim_subset)-1)*5+as.numeric(sim$draw)
  sim$id <- factor(sim$id)
  sim %<>% dplyr::rename(method = Method)
  sim %<>% dplyr::select(-Model, -Draw) %>%
    dplyr::mutate(eta = paste0("(", alpha, ", ", delta, ")")) %>%
    dplyr::mutate(alpha_delta_r = paste0("(", alpha, ", ", delta, ", ", r, ")"))
  
  sim$method <- factor(sim$method)
  sim %<>% arrange(desc(alpha),r,method) 
  sim$alpha_delta_r <- factor(sim$alpha_delta_r,
                              levels = unique(sim$alpha_delta_r))
  sim %<>%
    dplyr::rename(lambda = selected_lambda) 
  
  return(sim)
}

load_and_process_all <- function() {
  #maybe a trycatch here?  not sure what to do with this...
  list <- lapply(1:100, load_and_process_subset) 
  df <- data.table::rbindlist(list)
  return(df)
}

result <- load_and_process_all() # takes approximately 10 mintues.
readr::write_csv(result, "./efficacy_sims.csv")
rm(list = ls())
