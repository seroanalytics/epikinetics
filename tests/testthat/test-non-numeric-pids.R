test_that("Can convert character pids to numeric ids and back again", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

  dat$pid <- paste0("ID", dat$pid)
  lookup <- build_pid_lookup(dat)

  pids <- dat$pid
  dat[, nid := lookup[pid]]
  dat[, recovered := names(lookup)[nid]]

  expect_equal(dat$recovered, dat$pid)
})

test_that("Can convert numeric pids to numeric ids and back again", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

  lookup <- build_pid_lookup(dat)

  pids <- dat$pid
  dat[, nid := lookup[pid]]
  dat[, recovered := as.numeric(names(lookup)[nid])]

  expect_equal(dat$recovered, dat$pid)
})

test_that("Using numeric and non-numeric pids gives the same answer", {
  # these take a while, so don't run on CI
  skip_on_ci()
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
  stan_data <- mod$get_stan_data()

  dat$pid <- paste0("ID", dat$pid)
  mod_new <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
  stan_data_new <- mod_new$get_stan_data()

  expect_equivalent(stan_data, stan_data_new)

  fit <- mod$fit(parallel_chains = 4,
          iter_warmup = 50,
          iter_sampling = 100,
          seed = 100)

  fit_new <- mod_new$fit(parallel_chains = 4,
          iter_warmup = 50,
          iter_sampling = 100,
          seed = 100)

  expect_equivalent(fit$draws(), fit_new$draws())

  set.seed(1)
  params <- mod$extract_individual_parameters(100)

  set.seed(1)
  params_new <- mod_new$extract_individual_parameters(100)

  params$pid <- paste0("ID", params$pid)
  expect_equivalent(params, params_new)

  set.seed(1)
  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE,
                                                       n_draws = 100)

  set.seed(1)
  trajectories_new <- mod_new$simulate_individual_trajectories(summarise = FALSE,
                                                               n_draws = 100)
  trajectories$pid <- paste0("ID", trajectories$pid)
  trajectories <- dplyr::arrange(trajectories, pid)
  expect_equivalent(trajectories, trajectories_new)
})
