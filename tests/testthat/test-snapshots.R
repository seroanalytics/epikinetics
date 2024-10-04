dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
mod <- biokinetics$new(data = dat, covariate_formula = ~0 + infection_history)
suppressWarnings({delta <- mod$fit(parallel_chains = 4,
                 iter_warmup = 50,
                 iter_sampling = 100,
                 threads_per_chain = 4,
                 seed = 100)})

local_edition(3)

test_that("Model fits are the same", {
  expect_snapshot(delta)
})

test_that("Population trajectories are the same", {
  set.seed(1)
  trajectories <- mod$simulate_population_trajectories()
  expect_snapshot(trajectories)
})

test_that("Individual trajectories are the same", {
  set.seed(1)
  trajectories <- mod$simulate_individual_trajectories()
  expect_snapshot(trajectories)
})
