test_that("priors are named, inspectable, and converted in a stable order", {
  priors <- epikinetics_priors(time_to_peak = c(12, 3))
  expect_s3_class(priors, "epikinetics_priors")
  expect_equal(priors$population$time_to_peak, c(mean = 12, sd = 3))

  stan <- epikinetics:::priors_to_stan(priors)
  expect_equal(
    stan$population_prior_mean,
    c(6, 12, 50, 0.25, 0.02, 0.002)
  )
  expect_length(stan$participant_sd_prior_scale, 6)
  expect_length(stan$covariate_prior_scale, 6)
})

test_that("invalid prior shapes and scales are rejected", {
  expect_error(epikinetics_priors(baseline = 1), "c\\(mean, sd\\)")
  expect_error(epikinetics_priors(time_to_peak = c(10, 0)), "must be positive")
  expect_error(
    epikinetics_priors(late_waning_rate = c(-0.01, 0.02)),
    "must be non-negative"
  )
  expect_error(epikinetics_priors(observation_sd = -1), "positive")
  expect_error(
    epikinetics_priors(participant_sd = c(baseline = 1)),
    "invalid names"
  )
})
