mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Cannot retrieve population params until model is fitted", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$extract_population_parameters(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Cannot retrieve individual params until model is fitted", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$extract_individual_parameters(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Can extract population parameters", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  mod$fit()
  params <- mod$extract_population_parameters()
  expect_equal(names(params), c("k", "p", ".draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop",
                                "beta_t0", "beta_tp", "beta_ts", "beta_m1", "beta_m2", "beta_m3"))
})

test_that("Can extract individual parameters", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  mod$fit()
  params <- mod$extract_individual_parameters(n_draws = 10)
  expect_equal(names(params), c("stan_id", "titre_type", "draw", "t0_ind", "tp_ind", "ts_ind", "m1_ind", "m2_ind", "m3_ind"))
})
