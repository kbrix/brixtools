q <- pnorm(normal_poisson_n$deductible, lower.tail = FALSE)

o <- optim(c(1, 1, 1),
           normal_poisson_log_likelihood,
           control = list(fnscale = -1, maxit = 1e6),
           hessian = TRUE,
           X = normal_poisson_x, N = normal_poisson_n)

b <- brix_simple(optimizer = optim,
                 fn = normal_poisson_log_likelihood,
                 par = c(1, 1, 1),
                 control = list(fnscale = -1, maxit = 1e6),
                 hessian = TRUE,
                 X = normal_poisson_x, N = normal_poisson_n)

test_that("brix_simple outputs correctly", {
  #
  expect_output(str(b$par), "Named num")
  #
  expect_true(class(b$hessian) == "matrix")
})

test_that("Estimates are reasonable", {
  #
  expect_equal(o$par[3]*mean(q),
               0.1, tolerance = 0.01)
  #
  expect_equal(unname(b$par[3]*mean(q)), # removes name first
                      o$par[3]*mean(q))
})

