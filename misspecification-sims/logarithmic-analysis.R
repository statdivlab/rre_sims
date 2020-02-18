library(tidyverse)

all_results <- list.files("../sdl-rre/output", full.names=T) %>%
  str_subset("logarithmic_r_") %>%
  sapply(read_csv, simplify=F) %>%
  do.call(bind_rows, .)

all_results
cleaned_results <- all_results %>%
  select(-name, -label, 
         -Draw, -time) %>%
  mutate(Method = recode(Method, mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, replicates, p_log) 

cleaned_results 

cleaned_results %>%
  filter(Method == "Method 3") %>% 
  summarise(n(), 
            median(selected_lambda),
            mean(selected_lambda == 140),
            mean(selected_lambda == 0)) %>%
  print(n = Inf)

cleaned_results %>%
  filter(Method == "Method 3") %>%
  mutate(zl = selected_lambda == 0) %>%
  ungroup %>%
  summarise(mean(zl), n())

# Lambda limit is no problem here

cleaned_results %>%
  summarise(n())

cleaned_results %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n())  %>%
  arrange(replicates, p_log, ccc, Method) # identical

cleaned_results$p_log %>% unique
cleaned_results$replicates %>% unique
cleaned_results$ccc %>% unique

all_results %>%
  select(-name, -label, -time) %>%
  pivot_wider(id_cols=c(ccc, replicates, p_log, Draw), 
              names_from=Method, values_from=ccc_hat, 
              values_fn = list(ccc_hat = mean)) %>%
  filter(gof == mle_no_pen)

## the key table
main_summary <- cleaned_results %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  # filter(n > 90) %>%
  select(-n) %>%
  filter(Method %in% c("Method 0", "Method 3")) %>%
  group_by(ccc, p_log, replicates) %>%
  pivot_wider(names_from=replicates, values_from=RMSE) %>%
  arrange(desc(p_log), ccc, Method)
main_summary %>% print(n=Inf)
