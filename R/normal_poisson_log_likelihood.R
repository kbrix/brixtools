#' @title
#' Normal-Poisson log-likelihood
#'
#' @description
#' Implementation of Normal-Poisson log-likelihood, taking deductibles into account.
#'
#' @param parameter \code{numeric}, vector of parameters of length 3 consisting of the mean and standard
#' deviation of the Normal component and the mean of the Poisson component. Default is \code{NULL}.
#' @param X \code{data.frame}, must have two columns named \code{claim} and \code{deductible}
#' for the Normal component of the log-likelihood. Default is \code{NULL}.
#' @param N \code{data.frame}, must have two columns \code{occurrence} and \code{deductible}
#' for the Poisson component of the log-likelihood. Default is \code{NULL}.
#' @param check default value is 1 and returns the log-likelihood, else returns the parameter
#' names of the model: \code{mean}, \code{sd} and \code{lambda}.
#'
#' @details
#' Note that the row number of \code{X} and \code{N} need not be the same.
#'
#' @return
#' Returns a \code{numeric} value or \code{character} vector of parameter names depending on
#' whether \code{check} = 1 or not.
#'
#' @examples
#' normal_poisson_log_likelihood(c(1,2,3), X = normal_poisson_x, N = normal_poisson_n, check = 1)
#' normal_poisson_log_likelihood(c(1,2,3), X = normal_poisson_x, N = normal_poisson_n, check = 0)
#' normal_poisson_log_likelihood(check = 0)
#'
#' @export
normal_poisson_log_likelihood <- function(parameter = NULL, X = NULL, N = NULL, check = 1) {

  # Define the parameters
  if(check == 1) {
    mean <- parameter[length(parameter)-2]
    sd <- parameter[length(parameter)-1]
    lambda <- parameter[length(parameter)]

    # Claim distribution
    x <- stats::dnorm(X$claim, mean = mean, sd = sd, log = T) - stats::pnorm(X$deductible, mean = mean, sd = sd, lower.tail = FALSE, log.p = TRUE)

    # Thinning factor
    q <- stats::pnorm(N$deductible, mean = mean, sd = sd, lower.tail = FALSE)

    # Occurrence distribution
    # Making sure lambda is defined
    if(lambda < 0) {
      lambda <- 1e10
    }
    n <- stats::dpois(N$occurrence, lambda = q * lambda, log = TRUE)

    # Log-likelihood
    ll <- (sum(x) + sum(n))

    # Making sure the log-likelihood is defined
    if(is.na(ll)) {
      ll <- -1e10
    } else if (ll == -Inf) {
      ll <- -1e10
    }

    return(ll) # PLUS

  } else {
    return(c("mean", "sd", "lambda"))
  }
}

#' @title
#' Simple optimizer
#'
#' @description
#' A simple optimizer for log-likelihoods or other functions.
#'
#' @param optimizer recommended choice is \code{optim}.
#' @param fn function to be optimized.
#' @param ... additional arguments passed to the \code{optimizer}, see \code{examples}.
#'
#' @details
#' The optimizer must always be specified first.
#' The function to be optimized must always be specified second.
#' See e.g. \code{\link{optim}} for the optimizer.
#'
#' @return
#' Returns an object of \code{\link{class}} "brix_simple". The function \code{summary} can
#' also be used to print a summary of the model.
#'
#' An object of class "brix" is a list containing at the function call and the results from
#' the optimizer. The optimal parameters are named if applicable. The summary contains at
#' least the following components:
#'
#' \item{call}{the function call.}
#' \item{coefficients}{the optimal coefficents.}
#' \item{value}{the optimal value of the log-likelihood function.}
#' \item{AIC}{the AIC of the log-likelihood.}
#'
#' @examples
#' # Using optim for optimization
#' model <- brix_simple(optimizer = optim,
#'                      # function to be optimized
#'                      fn = normal_poisson_log_likelihood,
#'                      # arguments passed to fn
#'                      X = normal_poisson_x, N = normal_poisson_n,
#'                      # additional arguments passed to optim
#'                      par = rep(1, 3),
#'                      control = list(fnscale = -1),
#'                      method = "Nelder-Mead",
#'                      hessian = TRUE)
#' model
#' summary(model)
#'
#' @export
brix_simple <- function(optimizer, fn, ...) {

  call <- match.call(expand.dots = TRUE)
  call[[1]] <- call$optimizer
  call$optimizer <- NULL
  info <- eval(call, parent.frame())

  fn <- call[[2]]; #fn
  names <- (do.call(eval(fn), list(check = 0)))
  names(info$par) <- names

  info$coefficients <- coef <- info$par
  info$call <- call

  class(info) <- "brix_simple"

  info
}

#' @export
print.brix_simple <- function(x, ...) {
  cat("Call used with brix_simple:\n")
  print(x$call)
  cat("\nOptimizer:\n")
  print(x[1:5])
}

#' @export
summary.brix_simple <- function(object, ...) {

  h <- object$hessian
  se <- sqrt(-diag(solve(h)))
  zval <- object$coef / se
  p <- 2 * stats::pnorm(-abs(zval))

  t <- cbind(Estimate = object$coef,
             `Std. Error` = se,
             `z value` = zval,
             `Pr(>|z|)` = p)

  AIC <- -2 * object$value + 2 * length(object$coef)

  r <- list(call = object$call,
            coefficients = t,
            value = object$value,
            AIC = AIC)

  class(r) <- "summary.brix_simple"

  r
}

#' @export
print.summary.brix_simple <- function(x, ...) {

  cat("Call used with brix_simple:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  stats::printCoefmat(x$coefficients, P.value = TRUE, has.Pvalue = TRUE)

  cat("Log-likelihood value:", x$value,"\tAIC:", x$AIC)

}
