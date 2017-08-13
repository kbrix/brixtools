#' @title
#' Augmentation of simple_brix model
#'
#' @description
#' Augments simple_brix model with unobserved claims (losses smaller than the deductibles)
#' for claims model distribution testing.
#'
#' @param object object of class "brix_simple", see \code{\link{brix_simple}}.
#'
#' @details
#' Using the Poisson distribution for augmentation of unobserved claims. (Might implement a Geometric method).
#'
#' @return
#' Returns an object of \code{\link{class}} "augment". Using the print command displays test results from both
#' the Shaprio-Wilks Normality test amd the Kolmogorov-Smirnov one-sample test for the standard normal
#' transformed augmented data. See \code{\link{shapiro.test}} and \code{\link{ks.test}}. Using the plot
#' command displays standard normal Q-Q plots for the standard normal transformed augmented data by default.
#'
#' @examples
#' b <- brix_simple(optimizer = optim,
#'                  fn = normal_poisson_log_likelihood,
#'                  par = c(1, 1, 1),
#'                  control = list(fnscale = -1, maxit = 1e6),
#'                  hessian = TRUE,
#'                  X = normal_poisson_x, N = normal_poisson_n)
#'
#' a <- augment(object = b)
#' print(a)
#' plot(a)
#' plot(a, augment = FALSE)
#'
#' @export
augment <- function(object) {

  if(object$model == "normal-poisson") {

    # Generate augmented lambda parameter

    f1 <- stats::pnorm(unlist(object$X["deductible"]),
                       mean = unlist(object$par["mean"]),
                       sd = unlist(object$par["sd"]))

    f2 <- stats::pnorm(unlist(object$X["deductible"]),
                       mean = unlist(object$par["mean"]),
                       sd = unlist(object$par["sd"]),
                       lower.tail = FALSE)

    augmented_lambda <- f1/f2

    # Generate augmented (unobserved) Poisson random variable (occurrence) for each (observed) claim

    augmented_poisson <- NULL
    for(i in 1:length(augmented_lambda)) {
      augmented_poisson[i] <- stats::rpois(1, lambda = augmented_lambda[i])
    }

    # Create losses smaller than deductibles, these are the unobserved claims

    augmented_normal_list <- list()
    augmented_normal <- NULL

    for(i in 1:length(augmented_poisson)) {
      augmented_normal_list[[i]] <- truncdist::rtrunc(augmented_poisson[i], # truncated normal dist
                                                      spec = "norm",
                                                      mean = object$par["mean"],
                                                      sd = object$par["sd"],
                                                      a = -Inf, b = object$X["deductible"][[1]][i])

      x1 <- ifelse(sum(augmented_normal_list[[i]]) == 0,
                   NA,
                   augmented_normal_list[[i]]) # unobserved claims

      x2 <- ifelse(sum(augmented_poisson[i]) == 0,
                   NA,
                   rep(object$X["deductible"][[1]][i], augmented_poisson[i])) # match deductible to unobserved claims

      augmented_normal <- rbind(augmented_normal,
                                matrix(c(x1, x2), nrow = length(x1), ncol = 2)) # binding together
    }

    # Cleaning
    augmented_normal <- as.data.frame(augmented_normal)
    augmented_normal <- stats::na.omit(augmented_normal)
    colnames(augmented_normal) <- c("claim", "deductible")

    output <- list(augmented_normal = augmented_normal,
                   X = object$X,
                   coefficients = object$coefficients,
                   model = object$model)

    class(output) <- "augment"

    output
  }
}

#' @export
print.augment <- function(x, ...) {
  uniform <- stats::pnorm(c(unlist(x$augmented_normal["claim"]),
                             unlist(x$X["claim"])), # transform to U(0,1)
                           mean = unlist(x$coefficients["mean"]),
                           sd = unlist(x$coefficients["sd"]))
  normal <- stats::qnorm(uniform) #transform to N(0,1)

  sha <- stats::shapiro.test(normal)

  ks <- stats::ks.test(normal, "pnorm")

  output <- list(shapiro = sha,
                 ks = ks)
  print(output)
}

#' @export
plot.augment <- function(x, augment = TRUE, ...) {

  if(augment == TRUE) {
    uniform1 <- stats::pnorm(c(unlist(x$augmented_normal["claim"]),
                              unlist(x$X["claim"])), # transform to U(0,1)
                            mean = unlist(x$coefficients["mean"]),
                            sd = unlist(x$coefficients["sd"]))
    normal1 <- stats::qnorm(uniform1) #transform to N(0,1)

    stats::qqnorm(normal1, main = paste("Normal Q-Q plot for the\n augmented", x$model, "model")) # plotting
    graphics::abline(0, 1)

  } else if(augment == FALSE) {
    uniform2 <- stats::pnorm(unlist(x$X["claim"]), # transform to U(0,1)
                             mean = unlist(x$coefficients["mean"]),
                             sd = unlist(x$coefficients["sd"]))
    normal2 <- stats::qnorm(uniform2) #transform to N(0,1)

    stats::qqnorm(normal2, main = paste("Normal Q-Q plot for the\n unaugmented", x$model, "model")) #plotting
    graphics::abline(0, 1)
  }
}
