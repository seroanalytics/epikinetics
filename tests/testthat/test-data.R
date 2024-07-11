mock_model <- function(name, package) {
  list(sample = function(stan_dt, ...) stan_dt)
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Can construct stan data", {
  dt <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- scova$new(data = dt,
                 priors = scova_priors(),
                 covariate_formula = ~0 + infection_history,
                 preds_sd = 0.25,
                 time_type = "relative")
  # the fit function has been mocked above to return the stan inputs
  stan_dt <- mod$fit()
  expect_equal(stan_dt$N_events, 335)
  expect_equal(unlist(stan_dt$id), unname(unlist(dt[, "stan_id"])))
})

test_that("Can initialise file path data", {
  expect_true(inherits(scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                               priors = scova_priors()), "scova"))
})

test_that("Can provide data directly", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  expect_true(inherits(scova$new(data = dat,
                               priors = scova_priors()), "scova"))
})
