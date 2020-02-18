library(tidyverse)
library(breakaway)
pasolli_et_al <- openxlsx::read.xlsx("https://ars.els-cdn.com/content/image/1-s2.0-S0092867419300017-mmc4.xlsx", sheet = 2)
ft <- pasolli_et_al$`#.Samples` %>% make_frequency_count_table
ft %>% head
ft %>% breakaway %>% summary

m0 <- rre::unregularized_mle(list(ft))
# m1 <- rre::minimum_subset_distance(list(ft))
m3 <- rre::gof_criterion(list(ft))
# m4 <- rre::cv_replicates(list(ft))


m3$best
m0$best

m3
m3_b <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(20, 100, by= 5))
m3_b

m3_c <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(100, 1100, by= 200))
m3_c


m3_d <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(700, 1100, by= 50))
m3_d

m3_e <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(850, 950, by= 10))
m3_e

m3_f <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(900, 920, by= 5))
m3_f

m3_g <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(905, 915, by= 1))
m3_g

m3_h <- rre::gof_criterion(list(ft),
                           lambda_vec = seq(912, 914, by= 0.25))
m3_h

m3_i <- rre::gof_criterion(list(ft),
                           starts = data.frame(alpha = c(0.0102, 0.0123),
                                               delta = c(0.00647, 0.00650)),
                           lambda_vec = 913.5)
m3_i


m3_j <- rre::gof_criterion(list(ft),
                           starts = data.frame(alpha = c(0.0102, 0.0123, 0.0102, 0.0123),
                                               delta = c(0.00647, 0.00647, 0.00650, 0.00650)),
                           lambda_vec = seq(912, 914, by= 0.5))
m3_j # 90712, but on boundary with lambda = 914

m0$best # MLE is 98600
ft %>% breakaway::sample_richness # 4930

### noooo this is close to the multiplier!

expand.grid("alpha" = unique(c(0.0102, 0.0123, 0.005)),
            "delta" = unique(c(0.00647, 0.00650)))
m3_k <- rre::gof_criterion(list(ft),
                           starts = expand.grid("alpha" = unique(c(0.0102, 0.0123, 0.005)),
                                                "delta" = unique(c(0.00647, 0.00650))),
                           c_seq_len = 20,
                           multiplier = 40,
                           lambda_vec = seq(900, 1000, by= 20))
m3_k
# now lambda = 900

m3_l <- rre::gof_criterion(list(ft),
                           starts = expand.grid("alpha" = unique(c(0.005, 0.01, 0.015)),
                                                "delta" = unique(c(0.005, 0.006, 0.007))),
                           c_seq_len = 20,
                           multiplier = 40,
                           lambda_vec = seq(890, 920, by= 5))
m3_l

m3_m <- rre::gof_criterion(list(ft),
                           starts = expand.grid("alpha" = unique(c(0.003, 0.005, 0.010)),
                                                "delta" = unique(c(0.005, 0.006, 0.007))),
                           c_seq_len = 10,
                           multiplier = 100,
                           lambda_vec = seq(890, 920, by= 10))
m3_m

### Yikes, maximum likelihood on boundary
m0_b <- rre::unregularized_mle(list(ft), multiplier=100)
m0_b$best

m0_c <- rre::unregularized_mle(list(ft),
                               c_seq_len = 10,
                               multiplier=1000)
m0_c$best
552160/4930 # 112

m0_d <- rre::unregularized_mle(list(ft),
                               c_seq_len = 10,
                               multiplier=200,
                               starts = expand.grid("alpha" = unique(c(0.001, 0.003)),
                                                    "delta" = unique(c(0.005, 0.008))))
m0_d$best
331954/4930 # 67

m0_e <- rre::unregularized_mle(list(ft),
                               c_seq_len = 200,
                               multiplier=90,
                               starts = expand.grid("alpha" = unique(c(0.001, 0.003, 0.005)),
                                                    "delta" = unique(c(0.005, 0.008))))
m0_e$best # lh = 21378.42, Chat = 419447
419447/4930 # 85

m0_f <- rre::direct_optimise(fct=ft, lambda=0, penalty="h1",
                             multiplier=100,
                     search_scheme = "bisection",
                     starts = expand.grid("alpha" = unique(c(0.00233504)),
                                          "delta" = unique(c(0.006411948))))
m0_f$best # lh = 21378.42, Chat = 420056

m0_f <- rre::direct_optimise(fct=ft, lambda=0, penalty="h1",
                             multiplier=100,
                             search_scheme = "bisection",
                             starts = expand.grid("alpha" = unique(c(0.00233504)),
                                                  "delta" = unique(c(0.006411948))))
m0_f$best

m0_g <- rre::direct_optimise(fct=ft, lambda=0, penalty="h1",
                             c_seq_len=500,
                             multiplier=120,
                             search_scheme = "bisection",
                             starts = expand.grid("alpha" = unique(c(0.00233504)),
                                                  "delta" = unique(c(0.006411948))))
m0_g$best # lh = 21378.42, Chat = 420056, yes, ok, finally
420056/4930

####

# m3_l <- rre::gof_criterion(list(ft),
#                            starts = expand.grid("alpha" = unique(c(0.005, 0.01, 0.015)),
#                                                 "delta" = unique(c(0.005, 0.006, 0.007))),
#                            c_seq_len = 20,
#                            multiplier = 40,
#                            lambda_vec = seq(890, 920, by= 5))
m3_l
197200/4930 # 40, yikes

m3_m <- rre::gof_criterion(list(ft),
                           starts = expand.grid("alpha" = unique(c(0.003, 0.005, 0.010)),
                                                "delta" = unique(c(0.005, 0.006, 0.007))),
                           c_seq_len = 10,
                           multiplier = 100,
                           lambda_vec = seq(890, 920, by= 10))
m3_m
221850/4930 # 45, ok

m3_n <- rre::gof_criterion(list(ft),
                           starts = expand.grid("alpha" = unique(c(0.003, 0.005, 0.010)),
                                                "delta" = unique(c(0.005, 0.006, 0.007))),
                           c_seq_len = 100,
                           multiplier = 60,
                           lambda_vec = 900:910)
m3_n # lambda = 905
163587/3930 # 41, ok

m3_n$best
