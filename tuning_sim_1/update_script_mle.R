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

try(setwd("/home/students/payntera/IS_Wil/simulator/tuning_sim_1/"), silent = TRUE)
library(simulator);
library(methods);
library(magrittr);
library(rre);

source("update_functions.R")
update_method_mle(subset_number = num)
