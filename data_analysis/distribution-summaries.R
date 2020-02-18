library(breakaway)
simulate <- function(ccc, alpha, delta, replicates, nsim) {
  # this function must return a list of length nsim
  x <- replicate(nsim,
                 rnbinom(n = ccc*replicates,
                         size = alpha,
                         prob = (delta/(1+delta))) %>%
                   matrix(. ,nrow = ccc, ncol = replicates) %>%
                   split(.,col(.)) %>%
                   lapply(breakaway::make_frequency_count_table),
                 simplify = F)
  return(x)
}
sims <- simulate(1000, 0.1, 0.1, replicates=1000, nsim = 1)
1 - ((sims[[1]] %>%
  lapply(breakaway::sample_richness) %>% 
  alpha_estimates %>%
  summary %$% estimate) / 1000) %>% mean
dnbinom(x=1, 
        size = 0.1,
        prob = (0.1/(1+0.1))) 


sims[[1]] %>%
  lapply(function(x) {x$Freq[x$Var1 == 1]}) %>% unlist



sims2 <- simulate(1000, 0.01, 0.001, replicates=100, nsim = 1)
1 - ((sims2[[1]] %>%
        lapply(breakaway::sample_richness) %>% 
        alpha_estimates %>%
        summary %$% estimate) / 1000) %>% mean

dnbinom(x=1, 
        size = 0.01,
        prob = (0.001/(1+0.001))) 
1-(dnbinom(x=0:9, 
        size = 0.01,
        prob = (0.001/(1+0.001))) %>% sum %>% round(3))


sims2[[1]] %>%
  lapply(function(x) {max(x$Var1)}) %>% unlist %>% mean
