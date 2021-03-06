library(tidyverse)

## the analysis of data with the bigger lambda grid

# all_results <- list.files("sdl-rre/output", full.names=T) %>%
#   str_subset("biglambda") %>%
#   sapply(read_csv, simplify=F) %>%
#   do.call(bind_rows, .)
# write_csv(x=all_results, path="table4-fig4/table4-fig4-raw-data.csv")

all_results <- read_csv("table4-fig4/table4-fig4-raw-data.csv")
cleaned_results <- all_results %>%
  filter(ccc.x == ccc.y, alpha.x == alpha.y, delta.x == delta.y, replicates == r) %>%
  select(-ccc.y, -alpha.y, -delta.y, -name, -label, 
         -Draw, -replicates) %>%
  select(-absolute_error) %>%
  rename(alpha = alpha.x, delta = delta.x, ccc = ccc.x) %>%
  mutate(Method = recode(Method, mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, alpha, delta, r) 

cleaned_results 

cleaned_results %>%
  filter(Method == "Method 3") %>% 
  summarise(median(selected_lambda),
            mean(selected_lambda == 140)) %>%
  print(n = Inf)
# Lambda limit doesn't look like too much of a problem here

## the key table
main_summary <- cleaned_results %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  filter(n > 90) %>%
  select(-n) %>%
  filter(Method %in% c("Method 0", "Method 3")) %>%
  pivot_wider(names_from=r, values_from=RMSE) %>%
  arrange(desc(delta), desc(alpha), ccc, Method) %>%
  select(Method, ccc, alpha, delta, `6`, `10`, `14`:`50`)
main_summary %>% print(n=Inf)
main_summary %>%
  filter(!(alpha == 0.01 & delta == 1e-5)) %>%
  kableExtra::kable(format="latex", digits=c(rep(Inf, 4), rep(0, 5)), 
                    col.names=c("Method", "$C$", "$\\alpha$", 
                                "$\\delta$", "6", "10", "14", "30", "50"), 
                    escape = FALSE)

## Fig 4 
cleaned_results %>%
  ungroup %>%   
  filter(Method %in% c("Method 0", "Method 3")) %>%
  mutate(Method = ifelse(Method == "Method 0", "[0] MLE", "[3] GOF")) %>%
  filter(alpha == 0.1) %>%
  filter(delta %in% c(1e-3, 1e-5)) %>%
  # mutate(r = paste("r =", r)) %>%
  filter(r != 8) %>%
  mutate(r = factor(r, levels=as.character(c(6,10,14,30,50)))) %>%
  # filter(!is.na(r)) %>%
  ggplot(aes(x = r, y = ccc_hat, col = Method, fill = Method)) +
  facet_wrap(~delta*ccc, scales="free_y",
             labeller = labeller(delta = (function(x) paste0("eta = (0.1, ",x,")")),
                                 ccc = (function(x) paste0("C = ",x)))) +
  geom_boxplot(outlier.colour = NULL, alpha = 0.6) +
  geom_hline(aes(yintercept = ccc)) +
  theme_bw() +
  labs(y = latex2exp::TeX("$\\hat{C}$")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.15, end = 0.7125) +
  scale_color_viridis_d(option = "viridis", begin = 0.15, end = 0.7125) +
  theme_bw() +  
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x=element_blank(),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.placement = "outside",
        legend.position="bottom")
# ggsave("/Users/adwillis/software/rre_sims/rre_manuscript/jas-submitted-v2/images/method_sim_2.pdf")

# percentage numbers
cleaned_results %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  filter(r != 8, n > 90) %>%
  ungroup %>%
  group_by(alpha, delta) %>% 
  pivot_wider(values_from=RMSE, names_from=Method) %>%
  mutate(diff0 = 100*(`Method 3` - `Method 0`)/`Method 0`,
         diff3 = 100*(`Method 0` - `Method 3`)/`Method 3`) %>%
  summarise(round(mean(diff0)),
            round(mean(diff3)))


cleaned_results %>%
  filter(Method == "Method 3") %>%
  ungroup %>%
  filter(delta == 0.001, r != 8) %>%
  group_by(alpha, delta) %>%
  summarise(lambda = median(selected_lambda), 
            zl = round(100*mean(selected_lambda == 0)), 
            n = n()) %>%
  print(n = Inf)
  