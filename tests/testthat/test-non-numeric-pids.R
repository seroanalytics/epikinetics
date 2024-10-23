test_that("Using numeric and non-numeric pids gives the same answer", {
  # these take a while, so don't run on CI
  skip_on_ci()
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
  stan_data <- mod$get_stan_data()

  dat$pid <- paste0("ID", dat$pid)
  mod_new <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
  stan_data_new <- mod_new$get_stan_data()

  expect_equal(stan_data, stan_data_new, ignore_attr = TRUE)

  fit <- mod$fit(parallel_chains = 4,
          iter_warmup = 10,
          iter_sampling = 40,
          seed = 100)

  fit_new <- mod_new$fit(parallel_chains = 4,
          iter_warmup = 10,
          iter_sampling = 40,
          seed = 100)

  expect_equal(fit$draws(), fit_new$draws(), ignore_attr = TRUE)

  set.seed(1)
  params <- mod$extract_individual_parameters(100)

  set.seed(1)
  params_new <- mod_new$extract_individual_parameters(100)

  params$pid <- paste0("ID", params$pid)
  expect_equal(params, params_new, ignore_attr = TRUE)

  set.seed(1)
  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE,
                                                       n_draws = 100)

  set.seed(1)
  trajectories_new <- mod_new$simulate_individual_trajectories(summarise = FALSE,
                                                               n_draws = 100)
  trajectories$pid <- paste0("ID", trajectories$pid)
  trajectories <- dplyr::arrange(trajectories, pid)
  expect_equal(trajectories, trajectories_new, ignore_attr = TRUE)
})
