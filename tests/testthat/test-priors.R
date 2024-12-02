test_that("Can construct biokinetics priors", {
  priors <- biokinetics_priors(1, 2, 3, 4, 5, 6 , 7, 8, 9, 10, 11, 12)
  expect_s3_class(priors, "biokinetics_priors")
  expect_true(is.list(priors))
  expect_equal(unclass(priors), list("mu_t0" = 1, "mu_tp" = 2, "mu_ts" = 3,
                                     "mu_m1" = 4, "mu_m2" = 5, "mu_m3" = 6,
                                     "sigma_t0" = 7, "sigma_tp" = 8, "sigma_ts" = 9,
                                     "sigma_m1" = 10, "sigma_m2" = 11, "sigma_m3" = 12))
})
