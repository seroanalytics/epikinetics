test_that("Can initialise file path data", {
  expect_true(inherits(biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                                       priors = biokinetics_priors()), "biokinetics"))
})

test_that("Can provide data directly", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_true(inherits(biokinetics$new(data = dat,
                                       priors = biokinetics_priors()), "biokinetics"))
})

test_that("Can construct stan data", {
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
  expect_equal(stan_data$N_events, 335)
  expect_equal(stan_data$mu_t0, priors$mu_t0)
  expect_equal(stan_data$sigma_t0, priors$sigma_t0)
  expect_equivalent(stan_data$id, dat$pid)
})

test_that("All data is assumed uncensored if no censored column provided", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  dat$censored <- NULL
  mod <- biokinetics$new(data = dat)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$uncens_idx, 1:nrow(dat))
  expect_equal(stan_data$cens_lo_idx, integer())
  expect_equal(stan_data$cens_hi_idx, integer())
})

test_that("Can handle non-numeric pids", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  ids <- dat$pid
  dat$pid <- paste0("ID-", dat$pid)
  mod <- biokinetics$new(data = dat)
  stan_data <- mod$get_stan_data()
  expect_equivalent(stan_data$id, ids)
})
