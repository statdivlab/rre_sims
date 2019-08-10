library(magrittr)
library(rre)
setwd("/Users/alex/Dropbox/classGrad/IS_Wil/rre_sims/data_analysis/")

lakes <- readr::read_tsv("./lakes_data.txt") %>% as.data.frame()
lakes_metadata <- readr::read_tsv("./lakes_metadata.txt") %>% as.data.frame()

# Deciding which samples to extract:
lakes_metadata %>% 
  dplyr::group_by(Site, Years, Period) %>%
  dplyr::summarize(number = dplyr::n()) %>%
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

list_of_samples %>% lapply(.,rre::get_cc_max) 

s1_m0 <- rre::unregularized_mle(list_of_samples[[1]], 
                                multiplier = 200,
                                c_seq_len = 200) # Chat = 23904 which is 42*cc
s1_m0$best

s2_m0 <- rre::unregularized_mle(list_of_samples[[2]], 
                                multiplier = 200,
                                c_seq_len = 200) # Chat = 48500 which is 84*cc
s2_m0$best

s3_m0 <- rre::unregularized_mle(list_of_samples[[3]],
                                multiplier = 200,
                                c_seq_len = 200) # Chat = 34245 which is 47*cc




run_methods <- function(fct_sample, lambda_grid = seq(0,140,by=10),
                        multiplier = 20, # default for all methods.
                        c_seq_len = 96) {
  fct_sample %>%
    list(method_0 = rre::unregularized_mle(., multiplier = multiplier,
                                           c_seq_len = c_seq_len),
         method_1 = rre::minimum_subset_distance(.,
                                                 lambda_vec = lambda_grid,
                                                 partitions = 10,
                                                 multiplier = multiplier,
                                                 c_seq_len = c_seq_len),
         method_2 = rre::cv_replicates(.,  eval_function = "neg_unreg_like",
                                       lambda_vec = lambda_grid,
                                       partitions = 10,
                                       multiplier = multiplier,
                                       c_seq_len = c_seq_len),
         method_3 = rre::gof_criterion(., lambda_vec = lambda_grid,
                                       multiplier = multiplier,
                                       c_seq_len = c_seq_len),
         method_4 = rre::cv_replicates(.,  eval_function = "gof_chi_sq",
                                       lambda_vec = lambda_grid,
                                       partitions = 10,
                                       multiplier = multiplier,
                                       c_seq_len = c_seq_len))
}


# Now that we have a first guess for each we know the approximate C grid size, 
# first pass will be using a big lambdagrid and a C grid big enough for all three.



