library(tidyverse)
library(simulator)
library(rre)
library(stringr)
library(magrittr)
library(actuar)

logarithmic_FCT_list <- function(ccc, p_log, replicates) {
  new_model(name = sprintf("logarithmic_fct-list"),
            label = sprintf("Logarithmic draws from true C = %s, prob = %s, replicates = %s)", ccc, p_log, replicates),
            params = list(ccc = ccc, p_log = p_log, replicates = replicates),
            simulate = function(ccc, p_log, replicates, nsim) {
              # this function must return a list of length nsim
              x <- replicate(nsim,
                             (actuar::rlogarithmic(n = ccc*replicates,
                                                  prob = p_log) - 1) %>%
                               matrix(. ,nrow = ccc, ncol = replicates) %>%
                               split(.,col(.)) %>%
                               lapply(make_frequency_count_table),
                             simplify = F)
              return(x);
            })
}

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

# Method 0:
mle_no_pen <- new_method("mle_no_pen", "mle_no_pen",
                         method = function(model, draw) {
                           result <- rre::unregularized_mle(draw,
                                                            c_seq_len = 20,
                                                            multiplier = 4)
                           return(result)
                         }
)

# Method 3
gof <- new_method("gof", "gof",
                  method = function(model, draw) {
                    result <- rre::gof_criterion(draw,
                                                 lambda_vec = c(seq(0, 20, by=5),
                                                                seq(30, 50, by=10),
                                                                seq(70, 140, by=35)),
                                                 c_seq_len = 20,
                                                 multiplier = 4)
                    return(result)
                  }
)

### determine what is the right tuning parameters to choose
my_sim <- new_simulation(name = "logarithmic_sims",
                         label = "logarithmic_sims") %>%
  generate_model(make_model = logarithmic_FCT_list,
                 ccc =  as.list(c(1000, 2000)),
                 p_log = 0.9937285,
                 replicates = as.list(c(30, 14, 6)),
                 vary_along = c("ccc", "replicates")) %>%
  simulate_from_model(nsim = 10, index=1:7)


m <- model(my_sim, subset = "logarithmic_fct-list/ccc_1000/p_log_c015c9854ed46bc990d99b5c9a1fa8ad3c5c837e/replicates_30")
d <- draws(my_sim, subset = "logarithmic_fct-list/ccc_1000/p_log_c015c9854ed46bc990d99b5c9a1fa8ad3c5c837e/replicates_30", index = 1)


d@draws$r1.1 %>% lapply(breakaway::sample_richness) ## around 810
mle_no_pen@method(model = m, draw = d@draws$r1.1) ## estimate is 1484
gof@method(model = m, draw = d@draws$r1.1) ## estimate is 1484 and lambda = 50


my_sim %<>%
  run_method(list(mle_no_pen,
                  gof))
my_sim



my_sim %<>%
  run_method(list(mle_no_pen,
                  gof),
             parallel = list(socket_names = 7))

my_sim %<>%
  evaluate(list(ccc_hat, 
                selected_lambda))

ev_df <- my_sim %>% evals %>% as.data.frame
model_df <- my_sim %>% model %>% as.data.frame
ev_with_model_params <- dplyr::right_join(model_df, ev_df, by = c("name" = "Model"))

write_csv(ev_with_model_params, "/Users/adwillis/software/rre_sims/sdl-rre/output/logarithmic2.csv")

ev_with_model_params %>%
  as_tibble %>%
  filter(ccc.x == ccc.y, replicates == r) %>%
  select(-ccc.y, -name, -label, 
         -Draw, -replicates) %>%
  select(-absolute_error) %>%
  # rename(ccc = ccc.x) %>%
  mutate(Method = recode(Method, 
                         mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc.x, r)  %>%
  filter(Method == "Method 3") %>% 
  summarise(median(selected_lambda),
            mean(selected_lambda == 80)) %>%
  print(n = Inf)

# maximum selected lambda could be 60
# should increase this :(

ev_with_model_params %>%
  as_tibble %>%
  filter(ccc.x == ccc.y, replicates == r) %>%
  select(-ccc.y, -name, -label, 
         -replicates) %>%
  select(-absolute_error) %>%
  filter(Draw == "r1.1")

main_summary <- ev_with_model_params %>%
  as_tibble %>%
  filter(ccc.x == ccc.y, replicates == r) %>%
  select(-ccc.y, -name, -label, 
         -Draw, -replicates) %>%
  select(-absolute_error) %>%
  mutate(ccc = ccc.x) %>%
  mutate(Method = recode(Method, 
                         mle_no_pen = "Method 0",
                         gof = "Method 3")) %>%
  group_by(Method, ccc, r)  %>%
  summarise(RMSE = sqrt(mean((ccc_hat - ccc)^2)), 
            n = n()) %>%
  # filter(n > 90) %>%
  filter(Method %in% c("Method 0", "Method 3")) %>%
  pivot_wider(names_from=r, values_from=RMSE) %>%
  arrange(ccc, Method) %>%
  select(Method, ccc, `6`:`30`) %>%
  filter(!is.na(`6`))
main_summary
main_summary %>%
  kableExtra::kable(format="latex", digits=c(rep(Inf, 4), rep(0, 5)), 
                    col.names=c("Method", "$C$", "$\\alpha$", 
                                "$\\delta$", "6", "10", "14", "30", "50"), 
                    escape = FALSE)