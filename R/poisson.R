# Poisson log-likelihood

#' Poisson log-likelihood for regression purposes.
#'
#' Poisson log-likelihood for regression purposes to be used with \code{\link{brix}}, see also \code{\link{log_likelihood}}.
#'
#' @param beta numeric vector of parameters
#' @param z model/design matrix, see e.g. \code{\link{model.matrix}}
#' @param x vector of (non-negative integer) quantiles
#' @param link link function, the default is the logarithm, but the identity is also avalible
#' @param sum logical, default is TRUE
#' @name log_likelihood_poisson
#'
#' @return
#' The output is a numeric value, make sure the dimension of beta and z are legal for matrix multiplication, see the examples.
#'
#' @examples
#' x <- policy_data$claim
#'
#' log_likelihood_poisson(beta = c(0, 0),
#'                        z = model.matrix(claim ~ age, data = policy_data),
#'                        x = x)
#'
#' log_likelihood_poisson(beta = rep(1, length(x)),
#'                        z = diag(x),
#'                        x = x)
#'
#' do.call(what = log_likelihood(fam = poisson(link = "log"))$fun,
#'         args = list(beta = rep(1, length(x)), z = diag(x), x = x))
#' @export
NULL

#' @rdname log_likelihood_poisson
#' @export
log_likelihood_poisson <- function(beta, z, x, link = "log", sum = TRUE) {

    if (link == "log") {
        return(log_likelihood_poisson_log(beta = beta, z = z, x = x, sum = sum))
    }
    if (link == "identity") {
        return(log_likelihood_poisson_id(beta = beta, z = z, x = x, sum = sum))
    }
}

#' @rdname log_likelihood_poisson
#' @export
log_likelihood_poisson_log <- function(beta, z, x, sum = TRUE) {

    lambda <- exp(z %*% beta)

    if (sum == TRUE) {
        sum(log(stats::dpois(x = x, lambda = lambda)))
    } else {
        log(stats::dpois(x = x, lambda = lambda))
    }
}

#' @rdname log_likelihood_poisson
#' @export
log_likelihood_poisson_id <- function(beta, z, x, sum = TRUE) {

    lambda <- z %*% beta

    if (sum == TRUE) {
        sum(log(stats::dpois(x = x, lambda = lambda)))
    } else {
        log(stats::dpois(x = x, lambda = lambda))
    }
}
