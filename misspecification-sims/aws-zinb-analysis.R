library(tidyverse)
library(magrittr)
### ZINB

zinb_results <- read_csv("sdl-rre/output/zinb_ccc_1000_r_all_nsim_12_ncores_8.csv") %>%
  filter(ccc.x == ccc.y, alpha.x == alpha.y, delta.x == delta.y, replicates == r) %>%
  select(-ccc.y, -alpha.y, -delta.y, -name, -label, 
         -Draw, -replicates) %>%
  select(-absolute_error) %>%
  rename(alpha = alpha.x, delta = delta.x, ccc = ccc.x) %>%
  mutate(Method = recode(Method, mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, alpha, delta, zero_probability, r)  %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  filter(n > 90) %>%
  filter(zero_probability > 0) %>%
  select(-n) %>%
  filter(Method %in% c("Method 0", "Method 3")) %>%
  pivot_wider(names_from=r, values_from=RMSE) %>%
  arrange(zero_probability, Method) %>%
  ungroup %>%
  select(Method, alpha, delta, zero_probability, `6`:`50`) 

# zinb_results %>%
#   kableExtra::kable(format="latex", digits=c(rep(Inf, 2), rep(0, 5)), 
#                     col.names=c("Method", "$p$", "6", "10", "14", "30", "50"), 
#                     escape = FALSE)
# ## all with C = 1000, alpha = 0.1, delta = 0.001

# zinb_results %>% 
#   pivot_longer(cols=`6`:`50`) %>%
#   pivot_wider(names_from=Method, values_from=value) %>%
#   mutate(diff = 100*(`Method 0` - `Method 3`)/`Method 3`) %$%
#   diff %>%
#   mean

more_deltas <- list.files("sdl-rre/output/", full.names=TRUE) %>%
  str_subset("zinb_r") %>%
  sapply(read_csv, simplify=F) %>%
  do.call(bind_rows, .) %>%
  mutate(Method = recode(Method, mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, alpha, delta, zero_probability, replicates)  %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  filter(n > 90) %>%
  filter(zero_probability > 0) %>%
  select(-n) %>%
  # filter(Method %in% c("Method 0", "Method 3")) %>%
  pivot_wider(names_from=replicates, values_from=RMSE) %>%
  arrange(zero_probability, Method) %>%
  ungroup %>%
  select(Method, alpha, delta, zero_probability, `6`:`50`) 

more_deltas %>%
  full_join(zinb_results) %>%
  arrange(delta, zero_probability, Method) %>%
    kableExtra::kable(format="latex", digits=c(rep(Inf, 4), rep(0, 5)),
                      col.names=c("Method", "$\\alpha$", "$\\delta$", "$p$", "6", "10", "14", "30", "50"),
                      escape = FALSE)
