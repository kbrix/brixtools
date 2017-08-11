m <- 10000 # number of policy holders

normal_poisson_policy <- list() # initialization
deductible <- NULL
occurrence <- NULL
loss <- NULL
claim <- NULL

for(i in 1:m) { # loop for policy

  deductible <- rnorm(1, mean = 1, sd = 1)

  occurrence <- rpois(1, lambda = 0.1) # lambda = 0.1

  loss <- rnorm(occurrence, mean = 1.5, sd = 1.5) # mean = sd = 1.5

  claim <- loss[loss > deductible] # what we observe

  length <- length(claim)

  normal_poisson_policy[[i]] <- list(deductible = deductible,
                                     occurrence = occurrence,
                                     loss = loss,
                                     claim = claim,
                                     length = length)
}

# Occurrences with associated deductible
normal_poisson_n <- as.data.frame(cbind(occurrence = unlist(lapply(normal_poisson_policy, function(x) x$occurrence)),
                                        deductible = unlist(lapply(normal_poisson_policy, function(x) x$deductible)),
                                        length = unlist(lapply(normal_poisson_policy, function(x) x$length))))

# Claims with associated deductible
normal_poisson_x <- NULL
for(i in 1:m) {

  policy <- normal_poisson_policy[[i]]

  if(policy["length"] > 0) {

    claim <- policy$claim

    deductible <- rep(policy$deductible,
                      length(policy$deductible))

    normal_poisson_x <- as.data.frame(rbind(normal_poisson_x,
                                      cbind(claim, deductible)))
  }
}

# Saves data to correct folder
devtools::use_data(normal_poisson_policy, overwrite = TRUE)
devtools::use_data(normal_poisson_n, overwrite = TRUE)
devtools::use_data(normal_poisson_x, overwrite = TRUE)
