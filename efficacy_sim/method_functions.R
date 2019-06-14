make_fixed_lambda <- function(lambda) {
  new_method(name = sprintf("h1_lambda_%s", stringr::str_pad(format(lambda,nsmall = 2),6,pad = "0")),
             label = sprintf("lam = %s", stringr::str_pad(format(lambda,nsmall = 2),6,pad = "0")),
             method = function(model, draw) {
               wrapper_replicates(draw = draw,
                                  lambda = lambda)
             })
}

wrapper_replicates <- function(draw, lambda = 0) {
  print(paste0("Optimizing with lambda = ",lambda," at ",Sys.time()))
  result <- rre::fixed_lambda_mle(
    draw,
    lambda = lambda,
    starts = data.frame(alpha = c(1e-2,1e-2),
                        delta = c(1e-2,1e-4))
    )
  result$full <- NULL # deleting to save space
  result$full_starts <- NULL # deleting to save space
  return(result)
}

# In simulator lingo a "method" is a fixed value of lambda.
list_of_methods <- sapply(seq(0, 120, by = 5), make_fixed_lambda)

