library(magrittr)
library(rre)

get_n <- function(list_of_samples) {
  get_n_one_sample <- function(samp) samp[[1]] %>% length
  lapply(list_of_samples, get_n_one_sample) %>% unlist
}

# Temporary while running C grid sensitivity analysis:
get_cc <- function(list_of_results) {
  one_sample <- function(samp) samp[[1]] %>% rre::get_cc_max(.)
  lapply(list_of_results, one_sample) %>% unlist
}

get_estimate <- function(result) {
  best <- result %>% '[['(.,"best") 
  if ("ccc" %in% names(best)) {
    best[["ccc"]]
  } else if ("ccc_hat" %in% names(best)) {
    best[["ccc_hat"]]
  } else {
    stop("Estimate not found for this method")
  }
}

process_sample <- function(list_of_outputs) {
  list_of_outputs %<>% '['(., 2:6) # the first list item is just the data, so 2-6 are the 5 methods
  lapply(list_of_outputs, get_estimate) %>% unlist
}

process_sample_list <- function(list_of_sample_outs, 
                                sample_names = NA,
                                latex_format = T) {
  table_output <- lapply(list_of_sample_outs, process_sample) %>%
    do.call(cbind,.) 
  if (any(!is.na(sample_names))) {
    colnames(table_output) <- sample_names
  }
  
  if (latex_format) {
    r <- get_n(lakes_results)
    table_output %<>% rbind(.,r)
    rownames(table_output) <- c( paste0("$\\widehat{C}_{[", 0:4, "]}$"),  "$r$")
    table_output %>% xtable::xtable(digits = 0) %>% print(
      sanitize.text.function = identity,
      sanitize.rownames.function = identity,
      santitize.colnames.function = identity
    )
  } else { # console output
    table_output
  }
}


load("data_analysis_results.RData")
load("data_analysis_results_big_C.RData")

# latex table outputs:


#process_sample_list(lakes_results, 
#                    sample_names = 2009:2011,
#                    latex_format = T)

#process_sample_list(lakes_results, 
#                    sample_names = 2009:2011,
#                    latex_format = T)