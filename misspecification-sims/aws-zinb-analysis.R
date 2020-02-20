library(tidyverse)
library(magrittr)

### ZINB

# first_sim <- read_csv("sdl-rre/output/zinb_ccc_1000_r_all_nsim_12_ncores_8.csv") %>%
#   filter(ccc.x == ccc.y, alpha.x == alpha.y, delta.x == delta.y, replicates == r) %>%
#   select(-ccc.y, -alpha.y, -delta.y, -name, -label,
#          -Draw, -r, -alpha_hat, -delta_hat) %>%
#   select(-absolute_error) %>%
#   rename(alpha = alpha.x, delta = delta.x, ccc = ccc.x) 
# more_deltas <- list.files("sdl-rre/output/", full.names=TRUE) %>%
#   str_subset("zinb_r") %>%
#   sapply(read_csv, simplify=F) %>%
#   do.call(bind_rows, .) %>%
#   select(-name, -label, -Draw) %>%
#   full_join(first_sim)
# write_csv(x=more_deltas, path="misspecification-sims/zinb-raw-data.csv")
more_deltas2 <- read_csv(file="misspecification-sims/zinb-raw-data.csv")

first_sim_neater <- more_deltas2 %>%
  mutate(Method = recode(Method, mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, alpha, delta, zero_probability, replicates)  %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)),
            n = n()) %>%
  filter(n > 90) %>%
  filter(zero_probability > 0) %>%
  select(-n) %>%
  filter(Method %in% c("Method 0", "Method 3")) %>%
  pivot_wider(names_from=replicates, values_from=RMSE) %>%
  arrange(zero_probability, Method) %>%
  ungroup %>%
  select(Method, alpha, delta, zero_probability, `6`:`50`) %>%
  arrange(desc(delta), zero_probability, Method)


first_sim_neater %>%
  kableExtra::kable(format="latex", digits=c(rep(Inf, 4), rep(0, 5)),
                    col.names=c("Method", "$\\alpha$", "$\\delta$", "$p$", "6", "10", "14", "30", "50"),
                    escape = FALSE)
