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
                               return(best[["ccc_hat"]])
                             })

alpha_hat <- new_metric("alpha_hat", "alpha_hat",
                             metric = function(model,out) {
                               best <- out[["best"]]
                               return(best[["alpha_hat"]])
                             })
delta_hat <- new_metric("delta_hat", "delta_hat",
                             metric = function(model,out) {
                               best <- out[["best"]]
                               return(best[["delta_hat"]])
                             })

absolute_error <- new_metric("absolute_error", "absolute_error",
                   metric = function(model, out) {
                     ccc <- model$ccc
                     best <- out[["best"]]
                     ccc_hat <- best[["ccc_hat"]]
                     return(sqrt((ccc-ccc_hat)^2))
                   })




