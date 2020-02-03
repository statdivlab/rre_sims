## run to test it all works

library(tidyverse)
library(simulator)
library(rre)
library(stringr)
library(magrittr)
source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

my_sim <- new_simulation(name = "expand_test",
                         label = "expand_test") %>%
  generate_model(seed = 200103,
                 make_model = FCT_list,
                 ccc = 1000, 
                 eta = list(c(0.1,0.1),c(0.01,0.00001)),
                 replicates = as.list(c(6,10,14)),
                 vary_along = c("eta","replicates")) %>%
  simulate_from_model(nsim = 2) 

my_sim %<>%
  run_method(list(mle_no_pen,
                  minimum_subset_distance,
                  cv_likelihood,
                  gof,
                  cv_gof))
my_sim %<>%
  evaluate(list(ccc, alpha, delta, r,
                ccc_hat, absolute_error, 
                alpha_hat, delta_hat,
                selected_lambda)) 

my_df <- my_sim %>% 
  evals %>%
  as.data.frame
write_csv(my_df, "my_df.csv")