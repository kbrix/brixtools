#' Zero-modified Poisson family
#'
#' Family for use with \code{\link{brix}}, implementing regression for zero-modified Poisson data.
#'
#' @param link link function, the default is "log", but "identity" is also avalible
#'
#' @examples
#' zmpoisson(link = "log")
#' zmpoisson(link = "identity")
#' zmpoisson()
#'
#' @export
zmpoisson <- function(link = "log") {
  structure(list(family = "zmpoisson", link = link, name = "p0"), class = "family")
}

#' Zero-modified Poisson log-likelihood for regression purposes
#'
#' Zero-modified Poisson log-likelihood for regression purposes to be used with \code{\link{brix}}, see also \code{\link{log_likelihood}}.
#'
#' @param beta numeric vector of parameters
#' @param z model/design matrix, see e.g. \code{\link{model.matrix}}
#' @param x vector of (non-negative integer) quantiles
#' @param link link function, the default is the "log", but the "identity" is also avalible
#' @param sum logical, default is TRUE
#' @name log_likelihood_zmpoisson
#'
#' @return
#' The output is a numeric value, make sure the dimension of beta and z are legal for matrix multiplication, see the examples.
#'
#' @examples
#' x <- policy_data$claim
#'
#' log_likelihood_zmpoisson(beta = c(0, 0, 0.5), # 0.5 is the p0 parameter
#'                          z = model.matrix(claim ~ age, data = policy_data),
#'                          x = x)
#'
#' log_likelihood_zmpoisson(beta = c(rep(1, length(x)), 0.5),
#'                          z = diag(x),
#'                          x = x)
#'
#' do.call(what = log_likelihood(fam = zmpoisson(link = "log"))$fun,
#'         args = list(beta = c(rep(1, length(x)), 0.5), z = diag(x), x = x))
#' @export
NULL

#' @rdname log_likelihood_zmpoisson
#' @export
log_likelihood_zmpoisson <- function(beta, z, x, link = "log", sum = TRUE) {

  if(link == "log") {
    return(log_likelihood_zmpoisson_log(beta = beta, z = z, x = x, sum = sum))
  }
  if(link == "identity") {
    return(log_likelihood_zmpoisson_id(beta = beta, z = z, x = x, sum = sum))
  }
}

# Zero-modified Poisson log-likelihood with link = log ----------------------------------------

#' @rdname log_likelihood_zmpoisson
#' @export
log_likelihood_zmpoisson_log <- function(beta, z, x, sum = TRUE) {

  p0 <- beta[length(beta)]
  beta <- beta[-length(beta)]
  lambda <- exp(z %*% beta)

  s <- ifelse(x == 0 & lambda ==0, 0, x*log(lambda))
  l <- NA

  if (sum == TRUE && p0 > 0 && p0 < 1 #&& lambda > 0
  ) {

    t <- ifelse(x == 0,
      p0,
      log(1 - p0) + s - log(factorial(x)) - log(exp(lambda) - 1))
    l <- sum(t)

  }

  if (sum == FALSE && p0 > 0 && p0 < 1 #&& lambda > 0
  ) {

    l <- ifelse(x == 0,
      p0,
      log(1 - p0) + s - log(factorial(x)) - log(exp(lambda) - 1))
  }
  l #return log-likelihood
}

# Zero-modified Poisson log-likelihood with link = identity -----------------------------------

#' @rdname log_likelihood_zmpoisson
#' @export
log_likelihood_zmpoisson_id <- function(beta, z, x, sum = TRUE) {

  p0 <- beta[length(beta)]
  beta <- beta[-length(beta)]
  lambda <- z %*% beta

  s <- ifelse(x == 0 & lambda ==0, 0, x*log(lambda))
  l <- NA

  if (sum == TRUE && p0 > 0 && p0 < 1 #&& lambda > 0
  ) {

    t <- ifelse(x == 0,
      p0,
      log(1 - p0) + s - log(factorial(x)) - log(exp(lambda) - 1))
    l <- sum(t)

  }

  if (sum == FALSE && p0 > 0 && p0 < 1 #&& lambda > 0
  ) {

    l <- ifelse(x == 0,
      p0,
      log(1 - p0) + s - log(factorial(x)) - log(exp(lambda) - 1))
  }
  l #return log-likelihood
}
