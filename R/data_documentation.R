#' Claim data
#'
#' Claim data from insurance company
#'
#' @format A data frame with 424 observations (rows) and 9 variables (columns):
#' \describe{
#'   \item{claim}{Total loss amount, including the deductible in USD, after taking the logarithm}
#'   \item{deductible}{The deductible in USD, after taking the logarithm}
#'   \item{age}{Age of the ship in years}
#'   \item{brt}{Gross tonnage of the ship, after taking the logarithm}
#'   \item{dwt}{Deadweight tonnage of the ship, after taking the logarithm}
#'   \item{value}{Sum insured in USD, after taking the logrithm}
#'   \item{hp}{Horsepowers}
#'   \item{year}{Year of insurance}
#'   \item{code}{Type of ship, 1 is bulk and 2 is cargo}
#' }
"claim_data"

#' Policy data
#'
#' Policy data from insurance company
#'
#' @format A data frame with 6111 observations (rows) and 10 variables (columns):
#' \describe{
#'   \item{time}{Fraction of the year the policy is active, also called the exposure}
#'   \item{deductible}{The deductible in USD, after taking the logarithm}
#'   \item{age}{Age of the ship in years}
#'   \item{brt}{Gross tonnage of the ship, after taking the logarithm}
#'   \item{dwt}{Deadweight tonnage of the ship, after taking the logarithm}
#'   \item{value}{Sum insured in USD, after taking the logrithm}
#'   \item{hp}{Horsepowers}
#'   \item{year}{Year of insurance}
#'   \item{code}{Type of ship, 1 is bulk and 2 is cargo}
#'   \item{claim}{Number of claims on that policy that year}
#' }
"policy_data"
