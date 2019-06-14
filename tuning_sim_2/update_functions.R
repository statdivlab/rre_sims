# A series of helper functions used to run sims on cluster.
 
# Method 0:
update_method_mle <- function(subset_number) {
  source("method_functions.R")
  source("model_functions.R")
  source("eval_functions.R")
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  sim <- load_simulation( paste0("sim_subset_", str),
                          dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    run_method(mle_no_pen)
}
 
# Method 1:
update_method_min_subset_var <- function(subset_number) {
  source("method_functions.R")
  source("model_functions.R")
  source("eval_functions.R")
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  sim <- load_simulation( paste0("sim_subset_", str),
                          dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    run_method(minimum_subset_distance)
}

# Method 2:
update_method_cv_likelihood <- function(subset_number) {
  source("method_functions.R")
  source("model_functions.R")
  source("eval_functions.R")
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  sim <- load_simulation( paste0("sim_subset_", str),
                          dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    run_method(cv_likelihood)
}

# Method 3:
update_method_gof <- function(subset_number) {
  source("method_functions.R")
  source("model_functions.R")
  source("eval_functions.R")
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  sim <- load_simulation( paste0("sim_subset_", str),
                          dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    run_method(gof)
}

# Method 4:
update_method_cv_gof <- function(subset_number) {
  source("method_functions.R")
  source("model_functions.R")
  source("eval_functions.R")
  str <- stringr::str_pad(subset_number, width = 2, side = "left", pad = "0")
  sim <- load_simulation( paste0("sim_subset_", str),
                          dir = paste0("./sim_subsets/", str, "/"))
  sim %<>%
    run_method(cv_gof)
}
