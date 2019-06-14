# The purpose of this file is to create the simulation subset objects, with a
# logical file system.  Each subset contains 5 simulations of each method.  All
# subsets are run as separate jobs on a cluster, then combined in
# main_aggregation.R.
rm(list = ls())
setwd("/Users/alex/Dropbox/blups/code/rre_sims/tuning_sim_1")
library(simulator); library(rre); library(stringr);
source("model_functions.R")

generate_subsets <- function(subset_number) {
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  my_sim <- new_simulation(name = paste0("sim_subset_", str),
                           label = paste0("sim_subset_", str),
                           dir = paste0("./sim_subsets/", str)) %>%
    generate_model(seed = subset_number,
                   make_model = FCT_list,
                   ccc = 1000, # for testing quickly-ish
                   eta = list(c(0.1,0.1),c(0.01,0.00001)),
                   replicates = as.list(c(6,10,14)),
                   vary_along = c("eta","replicates")) %>%
    simulate_from_model(nsim = 5)
}

lapply(1:20, generate_subsets)

rm(list = ls())
