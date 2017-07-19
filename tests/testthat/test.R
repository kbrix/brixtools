library(testthat)

library(brixtools)

context("brix functionality")

b <- brix(start_value = c(0, 0, 0.1),
          ll = log_likelihood(fam = zmpoisson(link = 'log')),
          formula = claim ~ age,
          data = policy_data)

test_that("brix is working properly", {
  expect_output(str(b), "List of 16")
  expect_equal(length(b$par), 3)
  expect_equal(class(b$value), "numeric")
  expect_true(b$convergence == 0)
  expect_true(sum(class(unlist(b[c("df.null","df.residual", "null.deviance", "deviance", "AIC", "BIC")])) == rep("numeric", 6)) == 6)
})

