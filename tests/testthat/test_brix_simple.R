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

test_that("The log-likehood works", {
  #
  expect_true(class(normal_poisson_log_likelihood(c(1,2,3), X = normal_poisson_x, N = normal_poisson_n)) == "numeric")
  expect_true(class(normal_poisson_log_likelihood(c(1,2,3), X = normal_poisson_x, N = normal_poisson_n, check = 2)) == "character")
  expect_true(normal_poisson_log_likelihood(check = 3) == "normal-poisson")
})


test_that("brix_simple outputs correctly", {
  #
  expect_output(str(b$par), "Named num")
  #
  expect_true(class(b$hessian) == "matrix")
})

test_that("Estimates are reasonable", {
  #
  expect_equal(o$par[3],
               0.1, tolerance = 0.1)
  #
  expect_equal(unname(b$par["lambda"]), # removes name first
               o$par[3])
})

