library(magrittr)
library(rre)
setwd("/Users/alex/Dropbox/blups/code/rre_sims/")
lakes <- readr::read_tsv("./lakes_data.txt") %>% as.data.frame()
lakes_metadata <- readr::read_tsv("./lakes_metadata.txt") %>% as.data.frame()

# Deciding which samples to extract:
lakes_metadata %>% 
  dplyr::group_by(Site, Years, Period) %>%
  dplyr::summarize(number = n()) %>%
  dplyr::arrange(Site, Period, Years) %>%
  print(n = 500)

# Based on the above we want the following rows to obtain a series of samples:
lakes_metadata %>%
  dplyr::filter(Years %in% seq(2009, 2011), Period == "Summer", Site == "Littoral") %>%
  dplyr::arrange(Site, Years)

# sort(colnames(lakes))

# goal is to grab a set of samples from the same year and site
get_samples <- function(yr, zone, per = NULL, month = NULL) {
  quo_period <- dplyr::enquo(per)
  sample_list <- lakes_metadata %>%
    dplyr::filter(Site == zone) %>%
    dplyr::filter(Years == yr)
  if (!is.null(per)) {
    sample_list %<>%  dplyr::filter(Period == per) 
  }
  if (!is.null(month)) {
    sample_list %<>%  dplyr::filter(Months == month) 
  }
  sample_list %<>% dplyr::select(SampleID) %>% unlist
  tables <- lakes[,colnames(lakes) %in% sample_list] %>%
    lapply(., rre::make_frequency_count_table)
  return(tables)
}

# Note that one of the metadata files matches no lakes sample ID:  12.07.11.1
list_of_samples <- mapply(get_samples, seq(2009, 2011), "Littoral", "Summer")

run_methods <- function(fct_sample, lambda_grid = seq(0,140,by=10)) {
  fct_sample %>%
    list(method_0 = rre::unregularized_mle(.),
         method_1 = rre::minimum_subset_distance(.,
                                                 lambda_vec = lambda_grid,
                                                 partitions = 10),
         method_2 = rre::cv_replicates(.,  eval_function = "neg_unreg_like",
                                       lambda_vec = lambda_grid,
                                       partitions = 10),
         method_3 = rre::gof_criterion(., lambda_vec = lambda_grid),
         method_4 = rre::cv_replicates(.,  eval_function = "gof_chi_sq",
                                       lambda_vec = lambda_grid,
                                       partitions = 10))
}

lakes_results <- lapply(list_of_samples, run_methods)

# apples_results <- breakaway::apples %>%
#  list(.) %>%
#  list(method_0 = rre::unregularized_mle(.),
#        method_3 = rre::gof_criterion(., lambda_vec = lambda_grid))

save(lakes_results, file = "./data_analysis/data_analysis_results.RData")

lakes_results %>% lapply(. (function(y) lapply(y, function(x) x$best )))

lapply(list_of_samples, rre::get_cc_max)
756*20
