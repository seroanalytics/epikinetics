test_that("fit_epikinetics returns an S3 fit and forwards computation controls", {
  captured <- new.env(parent = emptyenv())
  prepared <- prepare_epikinetics_data(example_epikinetics_data())
  fake <- fake_epikinetics_fit(prepared)
  model <- list(sample = function(data, ...) {
    captured$data <- data
    captured$arguments <- list(...)
    fake$fit
  })
  local_mocked_bindings(
    compile_epikinetics_model = function(...) model,
    .package = "epikinetics"
  )

  fit <- fit_epikinetics(
    prepared,
    chains = 2,
    parallel_chains = 2,
    threads_per_chain = 3,
    grainsize = 1,
    iter_warmup = 10,
    iter_sampling = 20
  )
  expect_s3_class(fit, "epikinetics_fit")
  expect_equal(fit$computation$threads_per_chain, 3)
  expect_equal(captured$data$grainsize, 1)
  expect_equal(captured$arguments$iter_sampling, 20)
  expect_equal(captured$arguments$adapt_delta, 0.9)
  expect_equal(captured$arguments$max_treedepth, 12)
  expect_identical(cmdstan_fit(fit), fake$fit)
})

test_that("fit computation counts are validated", {
  prepared <- prepare_epikinetics_data(example_epikinetics_data())
  expect_error(
    fit_epikinetics(prepared, chains = 2, parallel_chains = 3),
    "cannot exceed"
  )
  expect_error(fit_epikinetics(prepared, threads_per_chain = 0),
               "positive integer")
  expect_error(fit_epikinetics(prepared, adapt_delta = 1),
               "strictly between")
})

test_that("fitting requires validated prepared model data", {
  data <- example_epikinetics_data()
  expect_error(fit_epikinetics(data), "prepare_epikinetics_data")

  prepared <- prepare_epikinetics_data(data)
  prepared$stan_data$biomarker[1] <- 99L
  expect_error(
    fit_epikinetics(prepared),
    "indices must be integers from 1 to N_biomarkers"
  )
})

test_that("failed CmdStan runs remain inspectable", {
  prepared <- prepare_epikinetics_data(example_epikinetics_data())
  failed <- list(
    return_codes = function() c(1L, 1L),
    output = function(...) "Initialization failed"
  )
  model <- list(sample = function(...) failed)
  local_mocked_bindings(
    compile_epikinetics_model = function(...) model,
    .package = "epikinetics"
  )

  expect_warning(
    fit <- fit_epikinetics(prepared, chains = 2, parallel_chains = 2),
    "preserves the CmdStanMCMC run"
  )
  expect_s3_class(fit, "epikinetics_failed_fit")
  expect_equal(fit$computation$sampling_state, "failed")
  expect_identical(cmdstan_fit(fit), failed)
  expect_equal(cmdstan_fit(fit)$output(), "Initialization failed")
})

test_that("summary works with posterior-draw test fits", {
  fit <- fake_epikinetics_fit()
  result <- summary(fit)
  expect_s3_class(result, "draws_summary")
  expect_true(any(grepl("population_baseline", result$variable)))
})
