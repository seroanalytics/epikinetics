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

  mod$fit(parallel_chains = 4,
          iter_warmup = 50,
          iter_sampling = 100,
          seed = 100)

  mod_new$fit(parallel_chains = 4,
          iter_warmup = 50,
          iter_sampling = 100,
          seed = 100)

  set.seed(1)
  params <- mod$extract_individual_parameters(100)

  set.seed(1)
  params_new <- mod_new$extract_individual_parameters(100)

  expect_equivalent(params, params_new)

  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE)
  trajectories_new <- mod_new$simulate_individual_trajectories(summarise = FALSE)

  expect_equivalent(trajectories[, -"pid"], trajectories_new[, -"pid"])
})
