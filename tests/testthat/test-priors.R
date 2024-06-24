test_that("Can construct named list of prior parameters", {
  priors <- epikinetics_priors(names = c("a", "b"), mu_values = c(0.1, 0.2), sigma_values = c(0.5, 0.6))
  expect_s3_class(priors, "epikinetics_priors")
  expect_true(is.list(priors))
  expect_equal(unclass(priors), list("mu_a" = 0.1, "mu_b" = 0.2, "sigma_a" = 0.5, "sigma_b" = 0.6))
})
