library(tidyverse)
library(breakaway)
library(VGAM)

N <- 1000; shape <- 0.5; y <- 1:N
proby <- dzipf(y, N = N, shape = shape)

plot(proby ~ y, type = "l", col = "blue", ylab = "Probability",
     ylim = c(0, 0.2), main = paste("Zipf(N = ",N,", shape = ",shape,")", sep = ""),
     lwd = 2, las = 1) 

sum(proby)  # Should be 1
max(abs(cumsum(proby) - pzipf(y, N = N, shape = shape)))  # Should be 0

N <- 1000; shape <- 0.5; y <- 1:N
(rzipf(2000, N, shape) - 1)  %>% make_frequency_count_table %>% head
(rzipf(2000, N, shape) - 1) %>% make_frequency_count_table %>% tail
(rzipf(2000, N, shape) - 1) %>% make_frequency_count_table %>% sample_richness
(rzipf(2000, N, shape) - 1) %>% make_frequency_count_table %>% breakaway
(rzipf(2000, N, shape) - 1) %>% make_frequency_count_table %>% list %>% rre::unregularized_mle



## logarithmic series MLE
p_xbar <- pasolli_et_al$`#.Samples` %>% mean
mle <- function(theta) {
  p_xbar + theta / ((1-theta)*log(1-theta))
}
mle(1e-5)
mle(1e-10)
mle(1-1e-10)
est <- uniroot(mle, interval=c(1e-10, 1-1e-10), maxiter=15)
est$root
mle(0.9937285)
library(actuar)
(rlogarithmic(2000, 0.9937285) - 1) %>% make_frequency_count_table
(rlogarithmic(2000, 0.9937285) - 1) %>% make_frequency_count_table %>% sample_richness
(rlogarithmic(1000, 0.9937285) - 1) %>% make_frequency_count_table %>% sample_richness
(rlogarithmic(2000, 0.9937285) - 1) %>% make_frequency_count_table %>% breakaway
(rlogarithmic(1000, 0.9937285) - 1) %>% make_frequency_count_table %>% breakaway

