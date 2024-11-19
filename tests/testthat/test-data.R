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
  expect_equal(names(stan_data), c("N", "N_events", "id", "value",
                                   "titre_type", "preds_sd", "K", "N_uncens", "N_lo",
                                   "N_hi", "uncens_idx", "cens_lo_idx",
                                   "cens_hi_idx", "t", "X", "P", "upper_censoring_limit", "lower_censoring_limit", "mu_t0",
                                   "mu_tp", "mu_ts", "mu_m1", "mu_m2", "mu_m3",
                                   "sigma_t0", "sigma_tp", "sigma_ts", "sigma_m1", "sigma_m2",
                                   "sigma_m3"))
  expect_equal(stan_data$N_events, 335)
  expect_equal(stan_data$mu_t0, priors$mu_t0)
  expect_equal(stan_data$sigma_t0, priors$sigma_t0)
  expect_equal(stan_data$id, dat$pid, ignore_attr = TRUE)
})

test_that("Data above/below limits is censored", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  suppressWarnings({ mod <- biokinetics$new(data = dat,
                                            lower_censoring_limit = 10,
                                            upper_censoring_limit = 2500) })
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$N_uncens, dat[value > 10 & value < 2500, .N])
  expect_equal(stan_data$N_lo, dat[value <= 10, .N])
  expect_equal(stan_data$N_hi, dat[value >= 2500, .N])
  expect_equal(stan_data$uncens_idx, dat[value > 10 & value < 2500, which = TRUE])
  expect_equal(stan_data$cens_lo_idx, dat[value <= 10, which = TRUE])
  expect_equal(stan_data$cens_hi_idx, dat[value >= 2500, which = TRUE])
})

test_that("Can handle non-numeric pids", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  ids <- dat$pid
  dat$pid <- paste0("ID-", dat$pid)
  mod <- biokinetics$new(data = dat)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$id, ids, ignore_attr = TRUE)
})

test_that("Natural scale data is converted to log scale for stan", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$value,
               convert_log2_scale(dat,
                                  smallest_value = 5,
                                  vars_to_transform = "value")$value,
               ignore_attr = TRUE)
})

test_that("Log scale data is passed directly to stan", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat, scale = "log")
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$value, dat$value, ignore_attr = TRUE)
})

test_that("Highest value is used as default upper limit", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat, lower_censoring_limit = 2)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$upper_censoring_limit, log2(max(dat$value) / min(dat$value)))
  expect_equal(stan_data$lower_censoring_limit, log2(2 / min(dat$value)))
})

test_that("Warns if data contains values above the upper limit and limit is strict", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_warning({ mod <- biokinetics$new(data = dat, upper_censoring_limit = 10) },
                 "Data contains values above the upper censoring limit 10 and these will be censored. To turn off this behaviour set strict_upper_limit to FALSE.")
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$upper_censoring_limit, log2(10 / min(dat$value)))
  expect_equal(stan_data$lower_censoring_limit, 0)
  expect_true(all(stan_data$value <= stan_data$upper_censoring_limit))
})

test_that("Warns if data contains values above the upper limit and limit is not strict", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_warning({ mod <- biokinetics$new(data = dat, upper_censoring_limit = 10, strict_upper_limit = FALSE) },
                 "Data contains values above the upper censoring limit 10. To treat these as censored set strict_upper_limit to TRUE.")
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$upper_censoring_limit, log2(10 / min(dat$value)))
  expect_equal(stan_data$lower_censoring_limit, 0)
  expect_false(all(stan_data$value <= stan_data$upper_censoring_limit))
})

test_that("Smallest value is used as default lower limit", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat, upper_censoring_limit = 3000)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$upper_censoring_limit, log2(3000 / min(dat$value)))
  expect_equal(stan_data$lower_censoring_limit, 0)
})

test_that("Warns if data contains values below the lower limit", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_warning({ mod <- biokinetics$new(data = dat,
                                          lower_censoring_limit = 10) },
    "Data contains values below the lower censoring limit 10 and these will be censored. To turn off this behaviour set strict_lower_limit to FALSE.")
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$lower_censoring_limit, log2(10/min(dat$value)))
  expect_true(all(stan_data$value >= stan_data$lower_censoring_limit))
})

test_that("Warns if data contains values below the lower limit and limit is not strict", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_warning({ mod <- biokinetics$new(data = dat, lower_censoring_limit = 10, strict_lower_limit = FALSE) },
                 "Data contains values below the lower censoring limit 10. To treat these as censored set strict_lower_limit to TRUE.")
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$lower_censoring_limit, log2(10/min(dat$value)))
  expect_false(all(stan_data$value <= stan_data$lower_censoring_limit))
})

test_that("Censoring limits are passed to stan without transformation if using log data", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = convert_log2_scale(dat,
                                                   smallest_value = 2,
                                                   vars_to_transform = "value"),
                         scale = "log",
                         lower_censoring_limit = 1,
                         upper_censoring_limit = 15)
  stan_data <- mod$get_stan_data()
  expect_equal(stan_data$upper_censoring_limit, 15)
  expect_equal(stan_data$lower_censoring_limit, 1)
})
