test_that("One of data or file required", {
  expect_error(scova$new(), "One of 'data' or 'file_path' must be provided")
})

test_that("Only one of data or file can be provided", {
  expect_error(scova$new(data = data.table::data.table(), file_path = "some/where"),
               "Only one of 'data' or 'file_path' should be provided")
})

test_that("Priors must be of type 'scova_priors'", {
  expect_error(scova$new(priors = list(t0 = 1, t2 = 3)),
               "'priors' must be of type 'scova_priors'")
})

test_that("preds_sd must be numeric", {
  expect_error(scova$new(preds_sd = "bad"),
               "'preds_sd' must be numeric")
})

test_that("Time type must be 'absolute' or 'relative'", {
  expect_error(scova$new(time_type = "bad"),
               "'time_type' must be one of 'relative' or 'absolute'")
})

test_that("Covariates must be present in data", {
  dat <- data.table(test = 1)
  expect_error(scova$new(data = dat, covariate_formula = 0~ bad),
               "All variables in 'covariate_formula' must correspond to data columns. Found unknown variables: bad")
})
