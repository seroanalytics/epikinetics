test_that("One of data or file required", {
  expect_error(biokinetics$new(), "One of 'data' or 'file_path' must be provided")
})

test_that("Only one of data or file can be provided", {
  expect_error(biokinetics$new(data = data.table::data.table(), file_path = "some/where"),
               "Only one of 'data' or 'file_path' should be provided")
})

test_that("Priors must be of type 'biokinetics_priors'", {
  expect_error(biokinetics$new(priors = list(t0 = 1, t2 = 3)),
               "'priors' must be of type 'biokinetics_priors'")
})

test_that("preds_sd must be numeric", {
  expect_error(biokinetics$new(preds_sd = "bad"),
               "'preds_sd' must be numeric")
})

test_that("Covariate formula must be a formula", {
  dat <- data.table(test = 1)
  expect_error(biokinetics$new(data = dat, covariate_formula = "bad"),
               "'covariate_formula' must be a formula")
})

test_that("Covariates must be present in data", {
  expect_error(biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"), covariate_formula = 0~ bad),
               "All variables in 'covariate_formula' must correspond to data columns. Found unknown variables: bad")
})

test_that("Required columns must be present in data", {
  dat <- data.table(test = 1)
  expect_error(biokinetics$new(data = dat, covariate_formula = 0~ bad),
               "Missing required columns: pid, day, last_exp_day, titre_type, value")
})

test_that("Data must be a data.table", {
  dat <- data.frame(test = 1)
  expect_error(biokinetics$new(data = dat),
               "'data' must be a data.table")
})
