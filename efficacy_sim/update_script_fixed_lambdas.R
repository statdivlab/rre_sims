#!/usr/local/bin/Rscript

args <- commandArgs(TRUE)
if (length(args) == 0) {
  print("You gave no args")
} else {
  for (i in 1:length(args)) {
    eval(parse(text = args[i]))
  }
}

if (!exists("num")) {
  stop("You did not input the right arguments")
}

setwd("/home/students/payntera/IS_Wil/simulator/efficacy_sim/")
library(simulator);
library(methods);
library(magrittr);
library(rre);

source("method_functions.R")
source("model_functions.R")
source("eval_functions.R")

str <- stringr::str_pad(num, width = 3, side = "left", pad = "0")
sim <- load_simulation( paste0("sim_subset_", str),
                        dir = paste0("./sim_subsets/", str, "/"))
# In this simulation a "method" is just the penalized MLE for replicates using a
# fixed value of lambda.  So list_of_methods is lambda = 0,5,..., 120.
sim %>%
  run_method(list_of_methods)

