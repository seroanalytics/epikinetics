mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Cannot retrieve trajectories until model is fitted", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$simulate_population_trajectories(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Can retrieve summarised trajectories", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE)
  expect_equal(names(trajectories), c("t", "p", "k", "me", "lo", "hi", "infection_history", "titre_type"))
})

test_that("Can retrieve un-summarised trajectories", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = FALSE)
  expect_equal(names(trajectories), c("t", "p", "k", ".draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop",
                                      "m3_pop", "beta_t0", "beta_tp", "beta_ts", "beta_m1", "beta_m2",
                                      "beta_m3", "mu", "infection_history", "titre_type"))
})
