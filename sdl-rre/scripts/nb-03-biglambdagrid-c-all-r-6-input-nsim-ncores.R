#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=4) {
  stop("NOPE! Bad args. ARGH!", call.=FALSE)
} else {
  input_ccc = as.numeric(args[1])
  input_r = as.numeric(args[2])
  input_nsim = as.numeric(args[3])
  input_ncores = as.numeric(args[4])
}

library(tidyverse)
library(simulator)
library(rre)
library(stringr)
library(magrittr)

FCT_list <- function(ccc, eta, replicates) {
  alpha <- eta[1]
  delta <- eta[2]
  new_model(name = sprintf("fct-list"),
            label = sprintf("NB draws from true C = %s, size = %s, prob = %s, replicates = %s)", ccc, alpha, delta, replicates),
            params = list(ccc = ccc, alpha = alpha, delta = delta, replicates = replicates),
            simulate = function(ccc, alpha, delta, replicates, nsim) {
              # this function must return a list of length nsim
              x <- replicate(nsim,
                             rnbinom(n = ccc*replicates,
                                     size = alpha,
                                     prob = (delta/(1+delta))) %>%
                               matrix(. ,nrow = ccc, ncol = replicates) %>%
                               split(.,col(.)) %>%
                               lapply(make_frequency_count_table),
                             simplify = F)
              return(x);
            })
}

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



# Fixed model parameters I want to return:
ccc <- new_metric("ccc", "ccc",
                  metric = function(model,out) {
                    return(model$ccc)
                  })

alpha <- new_metric("alpha", "alpha",
                    metric = function(model,out) {
                      eta <- model$eta
                      return(eta[1])
                    })

delta <- new_metric("delta", "delta",
                    metric = function(model,out) {
                      eta <- model$eta
                      return(eta[2])
                    })

r <- new_metric("r", "r",
                metric = function(model,out) {
                  r <- model$replicates
                  return(r)
                })

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

alpha_hat <- new_metric("alpha_hat", "alpha_hat",
                        metric = function(model,out) {
                          best <- out[["best"]]
                          if (!is.null(best[["alpha_hat"]])) {
                            ahat <- best[["alpha_hat"]]
                          } else if (!is.null(best[["alpha"]])) {
                            ahat <- best[["alpha"]]
                          } else {
                            stop("no alpha-hat?")
                          }
                          best <- out[["best"]]
                          return(ahat)
                        })

delta_hat <- new_metric("delta_hat", "delta_hat",
                        metric = function(model,out) {

                          best <- out[["best"]]
                          if (!is.null(best[["delta_hat"]])) {
                            dhat <- best[["delta_hat"]]
                          } else if (!is.null(best[["delta"]])) {
                            dhat <- best[["delta"]]
                          } else {
                            stop("no delta-hat?")
                          }
                          best <- out[["best"]]
                          return(dhat)

                        })

absolute_error <- new_metric("absolute_error", "absolute_error",
                             metric = function(model, out) {
                               ccc <- model$ccc
                               best <- out[["best"]]

                               if (!is.null(best[["ccc_hat"]])) {
                                 chat <- best[["ccc_hat"]]
                               } else if (!is.null(best[["ccc"]])) {
                                 chat <- best[["ccc"]]
                               } else {
                                 stop("no chat?")
                               }

                               return(sqrt((ccc-chat)^2))
                             })


my_sim <- new_simulation(name = "expand_test",
                         label = "expand_test") %>%
  generate_model(seed = 200103,
                 make_model = FCT_list,
                 ccc =  as.list(c(500, 1000, 2000)),
                 eta = list(c(0.1, 0.001),
                            c(0.1,0.00001),
                            c(0.01, 0.001),
                            c(0.01,0.00001)),
                 replicates = input_r,
                 vary_along = c("eta")) %>%
  simulate_from_model(nsim = input_nsim, index = 1:input_ncores)

my_sim %<>%
  run_method(list(mle_no_pen,
                  gof),
             parallel = list(socket_names = input_ncores))

my_sim %<>%
  evaluate(list(ccc, alpha, delta, r,
                ccc_hat, absolute_error,
                alpha_hat, delta_hat,
                selected_lambda))

ev_df <- my_sim %>% evals %>% as.data.frame
model_df <- my_sim %>% model %>% as.data.frame
ev_with_model_params <- dplyr::right_join(model_df, ev_df, by = c("name" = "Model"))

write_csv(ev_with_model_params, paste("nb_biglambdagrid_ccc_", input_ccc, "_r_", input_r, "_nsim_", input_nsim, "_ncores_", input_ncores, ".csv", sep = ""))
