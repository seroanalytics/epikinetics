test_that("Can create code snippet with priors", {
  expect_equal(prior_code(biokinetics_priors()),
               paste("biokinetics_priors(mu_t0 = 4, mu_tp = 10, mu_ts = 60, mu_m1 = 0.25,",
                 "mu_m2 = -0.02, mu_m3 = 0, sigma_t0 = 2, sigma_tp = 2, sigma_ts = 3,",
                 "sigma_m1 = 0.01, sigma_m2 = 0.01, sigma_m3 = 0.01)"))
})
