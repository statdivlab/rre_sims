#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=4) {
  stop("NOPE! Bad args. ARGH!", call.=FALSE)
} else {
  input_r = as.numeric(args[1])
  input_p = as.numeric(args[2])
  input_nsim = as.numeric(args[3])
  input_seed = as.numeric(args[4])
}

library(tidyverse)
library(simulator)
library(rre)
library(stringr)
library(magrittr)
library(actuar)

logarithmic_FCT_list <- function(ccc, p_log, replicates) {
  new_model(name = sprintf("logarithmic_fct-list"),
            label = sprintf("Logarithmic draws from true C = %s, prob = %s, replicates = %s)", ccc, p_log, replicates),
            params = list(ccc = ccc, p_log = p_log, replicates = replicates),
            simulate = function(ccc, p_log, replicates, nsim) {
              # this function must return a list of length nsim
              x <- replicate(nsim,
                             (actuar::rlogarithmic(n = ccc*replicates,
                                                   prob = p_log) - 1) %>%
                               matrix(. ,nrow = ccc, ncol = replicates) %>%
                               split(.,col(.)) %>%
                               lapply(make_frequency_count_table),
                             simplify = F)
              return(x);
            })
}

# Method metrics:

# if/else here is because I changed the method return wording from "optimal" to "selected".
selected_lambda <- new_metric("selected_lambda", "selected_lambda",
                              metric = function(model,out) {
                                best <- out[["best"]]
                                namen <- names(best)
                                if ("optimal_lambda" %in% namen) {
                                  rtn <- best[["optimal_lambda"]]
                                } else if ("selected_lambda" %in% namen) {
                                  rtn <- best[["selected_lambda"]]
                                } else {
                                  stop("Your 'out' object doesnt have the right names.")
                                }
                                return(rtn)
                              })

ccc_hat <- new_metric("ccc_hat", "ccc_hat",
                      metric = function(model,out) {
                        best <- out[["best"]]
                        if (!is.null(best[["ccc_hat"]])) {
                          chat <- best[["ccc_hat"]]
                        } else if (!is.null(best[["ccc"]])) {
                          chat <- best[["ccc"]]
                        } else {
                          stop("no chat?")
                        }
                        return(chat)
                      })

# Method 0:
mle_no_pen <- new_method("mle_no_pen", "mle_no_pen",
                         method = function(model, draw) {
                           result <- rre::unregularized_mle(draw,
                                                            c_seq_len = 20,
                                                            multiplier = 4)
                           return(result)
                         }
)

# Method 3
gof <- new_method("gof", "gof",
                  method = function(model, draw) {
                    result <- rre::gof_criterion(draw,
                                                 lambda_vec = c(seq(0, 20, by=5),
                                                                seq(30, 50, by=10),
                                                                seq(70, 140, by=35)),
                                                 c_seq_len = 20,
                                                 multiplier = 4)
                    return(result)
                  }
)

### determine what is the right tuning parameters to choose
my_sim <- new_simulation(name = "logarithmic_sims",
                         label = "logarithmic_sims") %>%
  generate_model(seed= paste(input_seed, input_r, sep = ""),
                 make_model = logarithmic_FCT_list,
                 ccc =  as.list(c(1000, 2000)),
                 p_log = input_p,
                 replicates = input_r,
                 vary_along = "ccc") %>%
  simulate_from_model(nsim = input_nsim)

my_sim %<>%
  run_method(list(mle_no_pen,
                  gof))

my_sim %<>%
  evaluate(list(ccc_hat, 
                selected_lambda))

ev_df <- my_sim %>% evals %>% as.data.frame
model_df <- my_sim %>% model %>% as.data.frame
ev_with_model_params <- dplyr::right_join(model_df, ev_df, by = c("name" = "Model"))

write_csv(ev_with_model_params, 
          paste("logarithmic_r_", input_r, 
                "_p_", input_p, 
                "_nsim_", input_nsim, 
                "_seed_", input_seed, ".csv", sep = ""))

# expand.grid(c(30, 14, 6), c(0.99, 0.995), 4, 1:12) %>%
#   as_tibble %>%
#   mutate(all = paste(Var1, Var2, Var3, Var4)) %>% 
#   arrange(Var4) %>%
#   select(all) %$% all  %>%
#   cat(sep="\n")

