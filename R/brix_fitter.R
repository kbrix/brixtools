
# Looping function for all model combinations --------------------------------------------

predictor <- c("age", "brt", "dwt", "value", "hp", "year", "code")
response <- "claim"

brix_fitter <- function(response, predictor, ll, data) {
    n <- length(predictor)
    l <- rep(list(0:1), n)
    m <- as.matrix(expand.grid(l))
    colnames(m) <- predictor

    x <- NULL
    formula <- list()

    for (i in 1:2^n) {
        for (j in 1:n) {
            if (m[i, j] == 1) {
                x[j] <- predictor[j]
            } else {
                x[j] <- NA
            }
        }
        formula[[i]] <- x
    }

    formula <- lapply(formula, stats::na.omit)

    f <- list()
    f[[1]] <- formula(paste(response, " ~ 1"), data = data)

    for (i in 2:2^n) {
        f[[i]] <- formula(paste(response, " ~ ", paste(formula[[i]], collapse = " + ")))
    }

    Z <- list()

    for (i in 1:2^n) {
        Z[[i]] <- stats::model.matrix(f[[i]], data = data)
    }

    pb <- utils::txtProgressBar(min = 0, max = 2^n, style = 3)
    b <- list()

    # start.time <- Sys.time()
    for (i in 1:2^n) {
        b[[i]] <- brix(start_value = c(rep(0, ncol(Z[[i]])), rep(0.1, length(ll$name))), ll = ll, response = response, design = Z[[i]], data = data)
        utils::setTxtProgressBar(pb, i)
    }
    # end.time <- Sys.time() time.taken <- end.time - start.time

    # cat('\nCalculation Time:', time.taken, 'seconds')

    b
}

# brix(start_value = rep(0.1, 2), log_likelihood = poisson_log_likelihood, response = response, design = model.matrix(claim ~ age, data =
# policy_data), data = policy_data)

# brix(start_value = rep(0.1, ncol(Z[[2]])), log_likelihood = poisson_log_likelihood, response = response, design = Z[[2]], data = policy_data)
# predictor <- c('age', 'brt', 'dwt')

# start.time <- Sys.time() fitter <- brix_fitter(response = response, predictor = predictor, log_likelihood(poisson(link = 'log')), data =
# policy_data) end.time <- Sys.time() time.taken <- end.time - start.time time.taken

# fitter[[8]]$coef fitter[[8]]$AIC k <- glm(claim ~ age + brt + dwt, data = policy_data, family = poisson('log')) k$coef AIC(k) head(fitter[[8]]$z)
# head(k$model)


# fitter[[40]]$coef fitter[[40]]$AIC k <- glm(claim ~ age + brt + dwt + year, data = policy_data, family = poisson('log')) k$coef AIC(k)
# head(fitter[[40]]$z) head(k$model)

# exp(fitter[[40]]$coef %*% colMeans(fitter[[40]]$z)) exp(k$coef %*% colMeans(fitter[[40]]$z)) mean(policy_data$claim)

# fitter[[128]]$coef fitter[[128]]$AIC k <- glm(claim ~ age + brt + dwt + value + hp + year + code, data = policy_data, family = poisson('log'))
# k$coef AIC(k) head(fitter[[128]]$z) head(k$model)

# summary(fitter[[128]]) summary(k)

# str(fitter[[1]]) lapply(fitter, '[[', 14) lapply(fitter, '$.data.frame', 'AIC')




# start.time2 <- Sys.time() fitter2 <- brix_fitter(response = response, predictor = predictor, log_likelihood(zmpoisson(link = 'log')), data =
# policy_data) end.time2 <- Sys.time() time.taken2 <- end.time2 - start.time2 time.taken2

# fitter2[[128]] a <- unlist(lapply(fitter2, '$.data.frame', 'BIC')) which.min(a)

# which(a %in% sort(a)[1:4])

# plot(unlist(lapply(fitter2, '$.data.frame', 'deviance')))
