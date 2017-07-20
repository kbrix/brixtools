#' @title
#' Fitting regression models using maximum likelihood
#'
#' @description
#' Estimates parameters by the method of maximum likelihood in the chosen model along with other useful quantities.
#'
#' @details
#' The maximization of the log-likelihood function specified is done using the \code{\link{optim}}.
#'
#' @param start_value real valued vector of start values for the parameters.
#' @param ll log-likelihood function, see \code{\link{log_likelihood}}.
#' @param formula a symbolic description of the of the model to be fitted, see 'Details' for more information.
#' @param response name of response variable
#' @param design design/model matrix, see e.g. \code{\link{model.matrix}}
#' @param data the dataset
#' @param ... additional arguments passed to optim
#'
#' @examples
#' brix(start_value = c(0, 0),
#'      ll = log_likelihood(fam = poisson(link = 'log')),
#'      formula = claim ~ age,
#'      data = policy_data)
#'
#' brix(start_value = c(1, 1),
#'      ll = log_likelihood(fam = poisson(link = 'identity')),
#'      formula = claim ~ age,
#'      data = policy_data)
#'
#' brix(start_value = c(0, 0, 0.1),
#'      ll = log_likelihood(fam = zmpoisson(link = 'log')),
#'      formula = claim ~ age,
#'      data = policy_data)
#'
#' brix(start_value = c(1, 1, 0.1),
#'      ll = log_likelihood(fam = zmpoisson(link = 'identity')),
#'      formula = claim ~ age,
#'      data = policy_data)
#'
#' brix(start_value = c(0, 0, 0.1),
#'      ll = log_likelihood(fam = zmpoisson(link = 'log')),
#'      response = 'claim',
#'      design = model.matrix(claim ~ age, data = policy_data),
#'      data = policy_data)
#'
#' @export
brix <- function(start_value, ll, formula = NULL, response = NULL, design = NULL, data, ...) {

    if (is.null(design) == TRUE) {
        # if no model is specified, then uses a model matrix
        f <- formula(x = formula, data = data)
        z <- stats::model.matrix(f, data = data)
        response_name <- all.vars(f[[2]])
        x <- data[[response_name]]  # extract response variable from data
        # z <- model.matrix(formula(x = formula, data = data), data = data)
    } else {
        z <- design
        x <- data[[response]]
    }

    # x <- data[[all.vars(f[[2]])]] # extract response variable from data

    o <- stats::optim(par = start_value, fn = ll$fun, control = list(fnscale = -1, maxit = 1e+06), hessian = TRUE, z = z, x = x, ...)

    # o$f <- f # formula DOES NOT WORK IF NO MODEL IS SPECIFIED
    # model matrix
    o$z <- z
    # data
    o$data <- data

    o$coef <- as.vector(o$par)  # beta coefficients
    names(o$coef) <- names(o$par) <- c(colnames(z), ll$name)  #names of beta coefficients

    # degrees of freedom in the null model, i.e. df_sat - df_null
    o$df.null <- nrow(o$data) - 1

    # degrees of freedom in the residual model, i.e. df_sat - df_proposed
    o$df.residual <- nrow(o$data) - length(o$coef)

    # null deviance
    z_null <- stats::model.matrix(claim ~ 1, data = data)

    extra <- abs(length(o$par) - ncol(o$z))

    null <- stats::optim(par = rep(0.1, 1 + extra), fn = ll$fun, control = list(fnscale = -1, maxit = 1e+06), z = z_null, x = x)

    id_family <- eval(parse(text = paste0(ll$info$family, "(link = \"identity\")")))
    id_family_log_likelihood <- do.call(log_likelihood, args = list(fam = id_family))

    if (extra == 0) {
        null_beta <- rep(1, length(x))
    } else {
        null_beta <- c(rep(1, length(x)), start_value[-(1:(length(start_value)) - start_value)])
    }

    o$null.deviance <- 2 * (do.call(id_family_log_likelihood$fun, args = list(x = x, beta = null_beta, z = diag(x)))
                           -do.call(ll$fun, args = list(x = x, beta = null$par, z = z_null)))


    o$deviance <- 2 * (do.call(id_family_log_likelihood$fun, args = list(x = x, beta = null_beta, z = diag(x)))
                     - do.call(ll$fun, args = list(x = x, beta = o$par, z = o$z)))

    # AIC
    o$AIC <- -2 * o$value + 2 * length(o$par)

    # BIC
    o$BIC <- -2 * o$value + log(nrow(data)) * length(o$par)

    # The 'brix' object
    class(o) <- "brix"
    o$call <- match.call()

    o
}


# Print of main function 'brix' -----------------------------------------------------------


#' @export
print.brix <- function(x, ...) {
    cat("Call:\n")
    print(x$call)

    cat("\nCoefficients:\n")
    print(x$coef)

    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. NULL);", x$df.residual, "Residual")
    cat("\nNull Deviance:\t  ", x$null.deviance)
    cat("\nResidual Deviance:", x$deviance)

    cat("\t\tAIC:", x$AIC, " \tBIC:", x$BIC)
}

# Summary of the main function 'brix' ---------------------------------------------------------

#' @export
summary.brix <- function(object, ...) {

    h <- object$hessian
    se <- sqrt(-diag(solve(h)))
    zval <- object$par/se
    p <- 2 * stats::pnorm(-abs(zval))

    t <- cbind(Estimate = object$coef, `Std. Error` = se, `z value` = zval, `Pr(>|z|)` = p)

    r <- list(call = object$call, coefficients = t)

    r$null.deviance <- object$null.deviance
    r$df.null <- object$df.null

    r$deviance <- object$deviance
    r$df.residual <- object$df.residual

    r$AIC <- object$AIC
    r$BIC <- object$BIC

    class(r) <- "summary.brix"

    r
}

#' @export
print.summary.brix <- function(x, ...) {
    cat("Call:\n")
    print(x$call)

    cat("\nCoefficients:\n")
    stats::printCoefmat(x$coefficients, P.value = TRUE, has.Pvalue = TRUE)

    cat("\nNull Deviance:\t  ", x$null.deviance, " on", x$df.null, " degrees of freedom")
    cat("\nResidual Deviance:", x$deviance, " on", x$df.residual, " degrees of freedom\n")

    cat("\nAIC:", x$AIC, " \tBIC:", x$BIC)
}


