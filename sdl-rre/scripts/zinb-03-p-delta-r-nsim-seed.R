#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=5) {
  stop("NOPE! Bad args. ARGH!", call.=FALSE)
} else {
  input_p = as.numeric(args[1])
  input_delta = as.numeric(args[2])
  input_r = as.numeric(args[3])
  input_nsim = as.numeric(args[4])
  input_seed = as.numeric(args[5])
}

library(tidyverse)
library(simulator)
library(rre)
library(stringr)
library(magrittr)

ZI_FCT_list <- function(ccc, eta, replicates, zero_probability) {
  alpha <- eta[1]
  delta <- eta[2]
  new_model(name = sprintf("fct-list"),
            label = sprintf("ZINB draws from true C = %s, size = %s, prob = %s, r = %s, zero_prob = %s)", ccc, alpha, delta, replicates, zero_probability),
            params = list(ccc = ccc, alpha = alpha, delta = delta, 
                          replicates = replicates, 
                          zero_probability = zero_probability),
            simulate = function(ccc, alpha, delta, replicates, zero_probability, nsim) {
              number_zeros <- round(ccc*replicates*zero_probability)
              x <- replicate(nsim,
                             rnbinom(n = ccc*replicates - number_zeros,
                                     size = alpha,
                                     prob = (delta/(1+delta))) %>%
                               c(rep(0, number_zeros), .) %>%
                               sample %>%
                               matrix(. , nrow = ccc, ncol = replicates) %>%
                               split(., col(.)) %>%
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
                                                            c_seq_len = 25,
                                                            multiplier = 10)
                           return(result)
                         }
)


# Method 3
gof <- new_method("gof", "gof",
                  method = function(model, draw) {
                    result <- rre::gof_criterion(draw,
                                                 lambda_vec = seq(0, 140, by=10),
                                                 c_seq_len = 25,
                                                 multiplier = 10)
                    return(result)
                  }
)


my_sim <- new_simulation(name = "zinb_sims",
                         label = "zinb_sims") %>%
  generate_model(seed = paste(input_seed, input_delta*1e5, input_p*100, input_r, sep = ""),
                 make_model = ZI_FCT_list,
                 ccc =  1000,
                 eta = c(1e-1, input_delta), 
                 zero_probability = input_p, 
                 replicates = input_r) %>%
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
          paste("zinb_r_", input_r, 
                "delta_", input_delta, 
                "p_", input_p, 
                "_nsim_", input_nsim, 
                "_seed_", input_seed, ".csv", sep = ""))

# expand.grid(c(0.1, 0.2, 0.3), 0.00001, c(50, 30), 5, 1:20) %>%
#   bind_rows(expand.grid(c(0.1, 0.2, 0.3), 0.00001, c(14, 10, 6), 10, 1:10)) %>%
#   bind_rows(expand.grid(0.3, 0.001, c(14, 10, 6), 10, 1:10)) %>%
#   bind_rows(expand.grid(0.3, 0.001, c(50, 30), 5, 1:20)) %>%
#   as_tibble %>%
#   arrange(Var5) %>%
#   mutate(all = paste(Var1, Var2, Var3, Var4, Var5)) %>% select(all) %$% all  %>%
#   cat(sep="\n")

