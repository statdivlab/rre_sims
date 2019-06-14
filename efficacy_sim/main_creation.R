# The purpose of this file is to create the simulation subset objects, with a
# logical file system.  Each subset contains 1 simulation of each lambda.  All
# 100 subsets are run as separate jobs on a cluster, then combined in
# main_aggregation.R.
rm(list = ls())
setwd("/Users/alex/Dropbox/blups/code/rre_sims/efficacy_sim/")
library(simulator); library(rre); library(stringr);
source("model_functions.R")

number_of_subsets <- 100
# this next line is just to name the folders e.g. 001, 002, ... instead of 1, 2, etc
subset_digit_length <- floor( log(number_of_subsets, base = 10) + 1)

generate_subsets <- function(subset_number) {
  str <- stringr::str_pad(subset_number, 
                          width = subset_digit_length, 
                          side = "left", pad = "0")
  my_sim <- new_simulation(name = paste0("sim_subset_", str),
                           label = paste0("sim_subset_", str),
                           dir = paste0("./sim_subsets/", str)) %>%
    generate_model(seed = subset_number,
                   make_model = FCT_list,
                   ccc = 1000, # for testing quickly-ish
                   eta = list( c(0.1,0.1), 
                               c(0.01,0.00001),
                               c(0.1, 0.001),
                               c(0.1, 0.00001) ),
                   replicates = as.list(c(6,10,14)),
                   vary_along = c("eta","replicates")) %>%
    simulate_from_model(nsim = 1)
}

lapply(1:100, generate_subsets) # Takes about 5 minutes.

