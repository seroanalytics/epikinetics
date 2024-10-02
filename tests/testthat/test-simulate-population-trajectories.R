mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Cannot retrieve trajectories until model is fitted", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$simulate_population_trajectories(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Validates inputs", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  expect_error(mod$simulate_population_trajectories(summarise = "bad"), "'summarise' must be logical")
  expect_error(mod$simulate_population_trajectories(n_draws = "bad"), "'n_draws' must be numeric")
  expect_error(mod$simulate_population_trajectories(time_type = "bad"), "'time_type' must be one of 'relative' or 'absolute'")
  expect_error(mod$simulate_population_trajectories(t_max = "bad"), "'t_max' must be numeric")
})

test_that("Can retrieve summarised trajectories", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE)
  expect_equal(names(trajectories), c("t", "me", "lo", "hi", "titre_type", "infection_history"))
})

test_that("Can retrieve un-summarised trajectories", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = FALSE)
  expect_equal(names(trajectories), c("t", ".draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop",
                                      "m3_pop", "beta_t0", "beta_tp", "beta_ts", "beta_m1", "beta_m2",
                                      "beta_m3", "mu", "titre_type", "infection_history"))
})

test_that("Absolute dates are returned if time_type is 'absolute'", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE, time_type = "absolute")
  expect_equal(class(trajectories$date), c("IDate", "Date"))
  expect_equal(trajectories$date, data.table::as.IDate("2021-01-29") + trajectories$t)
})

test_that("Only times up to t_max are returned", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE, t_max = 10)
  expect_true(all(trajectories$t <= 10))
})
