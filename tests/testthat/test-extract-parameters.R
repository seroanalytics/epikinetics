mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

test_that("Cannot retrieve population params until model is fitted", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$extract_population_parameters(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Cannot retrieve individual params until model is fitted", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$extract_individual_parameters(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Can extract population parameters without human readable covariates", {

  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )

  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  params <- mod$extract_population_parameters(n_draws = 10, human_readable_covariates = FALSE)
  expect_equal(names(params), c("p", "k", "draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop",
                                "beta_t0", "beta_tp", "beta_ts", "beta_m1", "beta_m2", "beta_m3"))
})

test_that("Can extract population parameters with human readable covariates", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  params <- mod$extract_population_parameters(n_draws = 10, human_readable_covariates = TRUE)
  expect_equal(names(params), c("draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop",
                                "beta_t0", "beta_tp", "beta_ts", "beta_m1", "beta_m2", "beta_m3",
                                "titre_type", "infection_history"))
})

test_that("Can extract individual parameters without human readable covariates", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  params <- mod$extract_individual_parameters(n_draws = 10,
                                              human_readable_covariates = FALSE,
                                              include_variation_params = FALSE)
  expect_equal(names(params), c("stan_id", "k", "draw", "t0_ind", "tp_ind", "ts_ind",
                                "m1_ind", "m2_ind", "m3_ind"))
})

test_that("Can extract individual parameters with human readable covariates", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  params <- mod$extract_individual_parameters(n_draws = 10,
                                              human_readable_covariates = TRUE,
                                              include_variation_params = FALSE)
  expect_equal(names(params), c("stan_id", "draw", "t0_ind", "tp_ind", "ts_ind",
                                "m1_ind", "m2_ind", "m3_ind", "titre_type"))
})

test_that("Can extract individual parameters with variation params", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )

  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  params <- mod$extract_individual_parameters(n_draws = 10,
                                              human_readable_covariates = TRUE,
                                              include_variation_params = TRUE)
  expect_equal(names(params), c("stan_id", "draw",
                                "t0_ind", "tp_ind", "ts_ind",
                                "m1_ind", "m2_ind", "m3_ind",
                                "z_t0", "z_tp", "z_ts",
                                "z_m1", "z_m2", "z_m3",
                                "titre_type"))
})
