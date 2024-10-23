test_that("Using relative and absolute dates gives the same answer", {
  # these take a while, so don't run on CI
  skip_on_ci()
  dat_absolute <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod_absolute <- biokinetics$new(data = dat_absolute, covariate_formula = ~0 + infection_history)
  delta_absolute <- mod_absolute$fit(parallel_chains = 4,
                   iter_warmup = 10,
                   iter_sampling = 75,
                   seed = 100)

  set.seed(1)
  trajectories_absolute <- mod_absolute$simulate_individual_trajectories(summarise = FALSE)

  dat_relative <- data.table::fread(test_path("testdata", "delta_full_relative.rds"))
  mod_relative <- biokinetics$new(data = dat_relative, covariate_formula = ~0 + infection_history)
  delta_relative <- mod_relative$fit(parallel_chains = 4,
                   iter_warmup = 10,
                   iter_sampling = 75,
                   seed = 100)

  set.seed(1)
  trajectories_relative <- mod_relative$simulate_individual_trajectories(summarise = FALSE)

  # convert relative days to absolute
  min_date <- min(dat_absolute$day)
  trajectories_relative$calendar_day <- min_date + trajectories_relative$calendar_day
  trajectories_relative$exposure_day <- min_date + trajectories_relative$exposure_day

  expect_equal(trajectories_relative, trajectories_absolute)

  population_trajectories_absolute <- mod_absolute$simulate_population_trajectories()
  population_trajectories_relative <- mod_relative$simulate_population_trajectories()

  expect_equal(population_trajectories_absolute, population_trajectories_relative)
})
