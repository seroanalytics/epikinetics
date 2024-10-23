if (fs::dir_exists("/epikinetics/bin/stan")) {
  # running in Docker, so recompile the models
  instantiate::stan_package_compile(
    models = instantiate::stan_package_model_files(path = "/epikinetics/bin/stan"),
    cpp_options = list(stan_threads = TRUE),
    stanc_options = list("O1"),
    force_recompile = TRUE
  )
}
dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
mod <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
delta <- mod$fit(parallel_chains = 4,
                 iter_warmup = 50,
                 iter_sampling = 100,
                 seed = 100)

test_that("Model fits are the same", {
  skip_on_ci()
  expect_snapshot(delta)
})

test_that("Population trajectories are the same", {
  skip_on_ci()
  set.seed(1)
  trajectories <- mod$simulate_population_trajectories()
  expect_snapshot(trajectories)
})

test_that("Individual trajectories are the same", {
  skip_on_ci()
  set.seed(1)
  trajectories <- mod$simulate_individual_trajectories()
  expect_snapshot(trajectories)
})
