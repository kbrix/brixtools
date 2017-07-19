# Generic log-likelihood -------------------------------------------------------------------------------------

#'  Generic log-likelihood for regression purposes.
#'
#'  Generic log-likelihood for regression purposes to be used with \code{\link{brix}}.
#'
#' @param fam a description of the the distribution and link function to specify the log-likelihood
#'
#' @details
#' The options for \emph{fam} is either poisson() or zmpoisson(), see \code{\link{log_likelihood_poisson}} and \code{\link{log_likelihood_zmpoisson}}
#'
#' @examples
#' x <- policy_data$claim
#'
#' do.call(what = log_likelihood(fam = poisson(link = 'log'))$fun,
#'         args = list(beta = rep(1, length(x)), z = diag(x), x = x))
#'
#' do.call(what = log_likelihood(fam = poisson(link = 'log'))$fun,
#'         args = list(beta = rep(1, length(x)), z = diag(x), x = x))
#'
#' model1 <- brix(start_value = c(0, 0),
#'                ll = log_likelihood(fam = poisson(link = 'log')),
#'                formula = claim ~ age,
#'                data = policy_data)
#' model1
#' summary(model1)
#'
#' model2 <- brix(start_value = c(0, 0, 0, 0, 0, 0.1),
#'                ll = log_likelihood(fam = zmpoisson(link = 'log')),
#'                formula = claim ~ age + brt + dwt + hp,
#'                data = policy_data)
#' model2
#' summary(model2)
#' @export
log_likelihood <- function(fam) {

    if (fam$family == "poisson" && fam$link == "log") {
        fun = log_likelihood_poisson_log
    }
    if (fam$family == "poisson" && fam$link == "identity") {
        fun = log_likelihood_poisson_id
    }

    if (fam$family == "zmpoisson" && fam$link == "log") {
        # not yet implemented
        fun = log_likelihood_zmpoisson_log
    }
    if (fam$family == "zmpoisson" && fam$link == "identity") {
        # not yet implemented
        fun = log_likelihood_zmpoisson_id
    }
    # do not delete
    objects <- list(info = fam, fun = fun, name = fam$name)  ##!!!
}

