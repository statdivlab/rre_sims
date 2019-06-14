# This is the model function for replicates
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