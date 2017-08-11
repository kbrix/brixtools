#' Normal-Poisson policy data
#'
#' Simulated Normal-Poisson policy data for testing and demostration purposes.
#'
#' @format The object \code{normal_poisson_policy} is a \code{list} with \eqn{m = 10000}{} policies.
#' Each policy consists of the following:
#'
#' \describe{
#'   \item{deductible}{Sample of standard Normal random variable of length 1.}
#'   \item{occurrence}{Sample of Poisson distributed random varible of length 1 with mean 1.}
#'   \item{loss}{Sample of Normal distributed random variable of length \code{occurrence},
#'   with mean 1.5 and standard deviation 1.5. Returns \code{numeric(0)} if \code{occurrence}
#'   is 0.}
#'   \item{claim}{The \code{loss} given that it is larger than the \code{deductible}.
#'   Returns \code{numeric(0)} if \code{occurrence} is zero or if all losses are smaller than
#'   the \code{deductible}.}
#'   \item{reported}{Length of \code{claim}. Returns zero if \code{claim} is \code{numeric(0)}.}
#' }
#'
#' The object \code{normal_poisson_n} is a \code{data.frame} with 10000 rows (policies) and 3
#' columns consisting of \code{occurrence}, \code{deductible} and \code{length}.
#'
#' The object \code{normal_poisson_x} is a \code{data.frame} with
#' \code{sum(normal_poisson_n$length)} rows and 2 columns consisting of
#' \code{claim} and \code{deductible}.
#'
"normal_poisson_policy"

#' @rdname normal_poisson_policy
"normal_poisson_n"

#' @rdname normal_poisson_policy
"normal_poisson_x"
