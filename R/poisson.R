# Poisson log-likelihood

#' Poisson log-likelihood for regression purposes.
#'
#' Poisson log-likelihood for regression purposes to be used with
#' \code{\link{brix}}, see also \code{\link{log_likelihood}}.
#'
#' @param beta \code{numeric} vector of parameters.
#' @param z \code{matrix}, model/design matrix, see e.g. \code{\link{model.matrix}}.
#' @param x \code{numeric} vector of (non-negative integer) quantiles.
#' @param d \code{numeric} vector of deductibles.
#' @param link link function, the default is the logarithm,
#' but the identity is also avalible.
#' @param sum \code{logical}, default is \code{TRUE}.
#' @name log_likelihood_poisson
#'
#' @return
#' The output is a \code{numeric} value. Make sure the dimension of
#' \code{beta} and \code{z} are legal for matrix multiplication, see the examples.
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
log_likelihood_poisson <- function(beta, z, x, d = 0, link = "log", sum = TRUE) {

    if (link == "log") {
        return(log_likelihood_poisson_log(beta = beta, z = z, x = x, sum = sum))
    }
    if (link == "identity") {
        return(log_likelihood_poisson_id(beta = beta, z = z, x = x, sum = sum))
    }
}

#' @rdname log_likelihood_poisson
#' @export
log_likelihood_poisson_log <- function(beta, z, x, d = 0, sum = TRUE) {

    lambda <- exp(z %*% beta)

    if (sum == TRUE) {
        sum(log(stats::dpois(x = x, lambda = lambda)))
    } else {
        log(stats::dpois(x = x, lambda = lambda))
    }
}

#' @rdname log_likelihood_poisson
#' @export
log_likelihood_poisson_id <- function(beta, z, x, d = 0, sum = TRUE) {

    lambda <- z %*% beta

    if (sum == TRUE) {
        sum(log(stats::dpois(x = x, lambda = lambda)))
    } else {
        log(stats::dpois(x = x, lambda = lambda))
    }
}
