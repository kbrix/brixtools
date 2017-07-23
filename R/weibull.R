#' Weibull family
#'
#' Family for use with \code{\link{brix}}, implementing regression for
#' Weibull data.
#'
#' @param link link function, the default is the logarithm,
#' but the identity is also avalible.
#'
#' @examples
#' weibull(link = "log")
#' weibull(link = "identity")
#' weibull()
#'
#' @export
weibull <- function(link = "log") {
  structure(list(family = "weibull", link = link, name = "gam"), class = "family")
}

#' Weibull log-likelihood for regression purposes
#'
#' Weibull log-likelihood for regression purposes to be used
#' with \code{\link{brix}}, see also \code{\link{log_likelihood}}.
#'
#' @param beta \code{numeric} vector of parameters.
#' @param z \code{matrix}, model/design matrix, see e.g. \code{\link{model.matrix}}.
#' @param x \code{numeric} vector of (non-negative integer) quantiles.
#' @param d \code{numeric} vector of deductibles.
#' @param link link function, the default is the logarithm, but the identity
#' is also avalible.
#' @param sum \code{logical}, default is \code{TRUE}.
#' @name log_likelihood_weibull
#'
#' @return
#' The output is a \code{numeric} value, make sure the dimension of
#' \code{beta} and \code{z} are legal for matrix multiplication, see the examples.
#'
#' @examples
#' x <- claim_data$claim
#'
#' log_likelihood_weibull(beta = c(0.5, 0.5, 2), # 2 is the gamma parameter
#'                          z = model.matrix(claim ~ age, data = claim_data),
#'                          x = x)
#'
#' log_likelihood_weibull(beta = c(rep(0, length(x)), 1),
#'                          z = diag(x),
#'                          x = x)
#'
#' do.call(what = log_likelihood(fam = weibull(link = "log"))$fun,
#'         args = list(beta = c(rep(0, length(x)), 1), z = diag(x), x = x))
#' @export
NULL

#' @rdname log_likelihood_weibull
#' @export
log_likelihood_weibull <- function(beta, z, x, d = 0, link = "log", sum = TRUE) {

  if(link == "log") {
    return(log_likelihood_weibull_log(beta = beta, z = z, x = x, d = 0, sum = sum))
  }
  if(link == "identity") {
    return(log_likelihood_weibull_id(beta = beta, z = z, x = x, d = 0, sum = sum))
  }
}

# Weibull log-likelihood with link = log ----------------------------------------

#' @rdname log_likelihood_weibull
#' @export
log_likelihood_weibull_log <- function(beta, z, x, d = 0, sum = TRUE) {

  gam <- beta[length(beta)] #gamma
  beta <- beta[-length(beta)] #beta

  a <- exp(z %*% beta) #regression, log


  t <-  gam*log(a) - (gam-1)*log(x) + a^(-gam)*(x^gam - d^gam)

  l <- length(x)*log(gam) - sum(t)

  l

}

# Weibull log-likelihood with link = identity -----------------------------------

#' @rdname log_likelihood_weibull
#' @export
log_likelihood_weibull_id <- function(beta, z, x, d = 0, sum = TRUE) {

  gam <- beta[length(beta)] #gamma
  beta <- beta[-length(beta)] #beta

  a <- z %*% beta #regression, id

  t <-  gam*log(a) - (gam-1)*log(x) + a^(-gam)*(x^gam - d^gam)

  l <- length(x)*log(gam) - sum(t)

  l

}

