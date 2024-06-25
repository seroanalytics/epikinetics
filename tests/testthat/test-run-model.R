mock_model <- function(name, package) {
  list(sample = function(...) "OK" )
}

mock_prepare_stan_data <- function(dt,
                                   priors,
                                   formula,
                                   time_type,
                                   preds_sd) {
  list(N = 0)
}

mockery::stub(run_model, "prepare_stan_data", mock_prepare_stan_data())
mockery::stub(run_model, "instantiate::stan_package_model", mock_model())

test_that("Can run model from file path data", {
  expect_equal(run_model(file_path = system.file("delta_trunc.rds", package = "epikinetics"),
                          priors = epikinetics_priors()), "OK")
})

test_that("Can run model by providing data directly", {
  expect_equal(run_model(data = data.table::data.table(),
                          priors = epikinetics_priors()), "OK")
})
