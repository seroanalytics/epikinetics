test_that("Can construct named list of Gaussian prior parameters", {
  priors <- gaussian_priors(names = c("a", "b"), mu_values = c(0.1, 0.2), sigma_values = c(0.5, 0.6))
  expect_s3_class(priors, "gaussian_priors")
  expect_true(is.list(priors))
  expect_equal(unclass(priors), list("mu_a" = 0.1, "mu_b" = 0.2, "sigma_a" = 0.5, "sigma_b" = 0.6))
})

test_that("Can construct cab prior parameters", {
  priors <- biokinetics_priors(mu_values = c(1, 2, 3, 4, 5, 6), sigma_values = c(7, 8, 9, 10, 11, 12))
  expect_s3_class(priors, "gaussian_priors")
  expect_s3_class(priors, "biokinetics_priors")
  expect_true(is.list(priors))
  expect_equal(unclass(priors), list("mu_t0" = 1, "mu_tp" = 2, "mu_ts" = 3,
                                     "mu_m1" = 4, "mu_m2" = 5, "mu_m3" = 6,
                                     "sigma_t0" = 7, "sigma_tp" = 8, "sigma_ts" = 9,
                                     "sigma_m1" = 10, "sigma_m2" = 11, "sigma_m3" = 12))
})
