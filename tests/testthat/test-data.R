mock_model <- function(name, package) {
  list(sample = function(stan_dt, ...) stan_dt)
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Can construct stan data", {
  dt <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dt,
                         priors = biokinetics_priors(),
                         covariate_formula = ~0 + infection_history,
                         preds_sd = 0.25)
  # the fit function has been mocked above to return the stan inputs
  stan_dt <- mod$fit()
  expect_equal(stan_dt$N_events, 335)
  expect_equal(unlist(stan_dt$id), unname(unlist(dt[, "pid"])))
})

test_that("Can initialise file path data", {
  expect_true(inherits(biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                                       priors = biokinetics_priors()), "biokinetics"))
})

test_that("Can provide data directly", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_true(inherits(biokinetics$new(data = dat,
                                       priors = biokinetics_priors()), "biokinetics"))
})

test_that("Can get stan data", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(mu_values = c(1, 2, 3, 4, 5, 6),
                               sigma_values = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
  mod <- biokinetics$new(data = dat, priors = priors)
  stan_data <- mod$get_stan_data()
  expect_true(is.list(stan_data))
  expect_equal(names(stan_data), c("N", "N_events", "id", "value", "censored",
                                   "titre_type", "preds_sd", "K", "N_uncens", "N_lo",
                                   "N_hi", "uncens_idx", "cens_lo_idx",
                                   "cens_hi_idx", "t", "X", "P", "mu_t0",
                                   "mu_tp", "mu_ts", "mu_m1", "mu_m2", "mu_m3",
                                   "sigma_t0", "sigma_tp", "sigma_ts", "sigma_m1", "sigma_m2",
                                   "sigma_m3"))
  expect_equal(stan_data$mu_t0, priors$mu_t0)
  expect_equal(stan_data$sigma_t0, priors$sigma_t0)
  expect_equal(stan_data$id, dat$pid)
})
