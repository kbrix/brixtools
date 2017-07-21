#' @title
#' Fitting regression models of all combinations of covariates using brix
#'
#' @description
#' Helper function for fitting regression models of all combinations of covariates
#' using brix, see \code{\link{brix}}.
#'
#' @details
#' See \code{\link{brix}}.
#'
#' @return
#' \code{brix_fitter} returns a \strong{list} of objects of \code{\link{class}} "brix".
#' Each object in the list is a model. If there are \eqn{n =} \code{length(predictor)}
#' covariates (excluding the intercept), then there are \eqn{2^n} different model
#' combinations. See \code{\link{brix}} for more infomation.
#'
#' @param response \code{character}, response variable name.
#' @param predictor \code{character}, vector of predictor variable names.
#' @param ll log-likelihood function, see \code{\link{log_likelihood}}.
#' @param data \code{data.frame}, the dataset used for regression.
#'
#' @examples
#' response <- "claim"
#'
#' ### Fitting Poisson log-link models (and measuring time):
#'
#' predictor <- c("age", "brt", "dwt")
#'
#' start.time1 <- Sys.time()
#' fit1 <- brix_fitter(response = response,
#'                     predictor = predictor,
#'                     log_likelihood(poisson(link = 'log')),
#'                     data = policy_data)
#' end.time1 <- Sys.time()
#' time.taken1 <- end.time1 - start.time1
#' time.taken1
#'
#' ### Comparison with glm
#' fit1[[8]]
#' glm(claim ~ age + brt + dwt, data = policy_data, family = poisson('log'))
#'
#' \dontrun{
#' ### Fitting zero-modified log-link models (takes several minutes to run)
#'
#' predictor <- c("age", "brt", "dwt", "value", "hp", "year", "code")
#'
#' start.time2 <- Sys.time()
#' fit2 <- brix_fitter(response = response,
#'                     predictor = predictor,
#'                     log_likelihood(zmpoisson(link = 'log')),
#'                     data = policy_data)
#' end.time2 <- Sys.time()
#' time.taken2 <- end.time2 - start.time2
#' time.taken2
#'
#' fit2[[128]]
#'
#' ### Extracting BIC values and finding the model with the smallest value
#' bic <- unlist(lapply(fit2, '$.data.frame', 'BIC'))
#' which.min(bic)
#'
#' which(bic %in% sort(lowest)[1:4])
#'
#' plot(bic)
#' }
#'
#' @export
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

    for (i in 1:2^n) {
        b[[i]] <- brix(start_value = c(rep(0, ncol(Z[[i]])), rep(0.1, length(ll$name))), ll = ll, response = response, design = Z[[i]], data = data)
        utils::setTxtProgressBar(pb, i)
    }
    b
}

