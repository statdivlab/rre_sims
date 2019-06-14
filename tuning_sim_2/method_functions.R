# Method 0
mle_no_pen <- new_method("mle_no_pen", "mle_no_pen",
                         method = function(model, draw) {
                           result <- rre::unregularized_mle(draw)
                           return(result)
                         }
)

# Method 1
minimum_subset_distance <- new_method("min_sub_dist", "Minimum Subset Distance",
                                      method = function(model, draw) {
                                        result <- minimum_subset_distance(draw,
                                                                          lambda_vec = seq(0,140,by = 10),
                                                                          partitions = 10)
                                        return(result)
                                      }
)

# Method 2
cv_likelihood <- new_method("cv_likelihood", "cv_likelihood",
                            method = function(model, draw) {
                              result <- rre::cv_replicates(draw,
                                                           eval_function = "neg_unreg_like",
                                                           lambda_vec = seq(0, 140, by=10))
                              return(result)
                            }
)

# Method 3
gof <- new_method("gof", "gof",
                  method = function(model, draw) {
                    result <- gof_criterion(draw,
                                            lambda_vec = seq(0, 140, by=10))
                    # parts and lambda_vec are default, just being explicit.
                    return(result)
                  }
)

# Method 4
cv_gof <- new_method("cv_gof", "cv_gof",
                     method = function(model, draw) {
                       result <- rre::cv_replicates(draw,
                                                    eval_function = "gof_chi_sq",
                                                    lambda_vec = seq(0, 140, by=10))
                       return(result)
                     }
)




