mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Cannot retrieve data until model is fitted", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$population_stationary_points(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Can get population stationary poins", {
  mod <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  result <- mod$population_stationary_points()
  expect_equal(names(result), c("infection_history", "titre_type", "mu_p", "mu_s", "rel_drop_me", "mu_p_me", "mu_s_me"))
})
