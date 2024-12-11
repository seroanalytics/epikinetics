test_that("Can plot prior prediction up to tmax", {
  priors <- biokinetics_priors()
  plot <- plot(priors, tmax = 100, n_draws = 500)
  expect_equal(nrow(plot$data), 100)
  expect_equal(length(plot$layers), 2)
})

test_that("Can plot prior prediction with data points", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors()
  expect_error(plot(priors, data = data), "Missing required columns: time_since_last_exp")
  data[, `:=`(time_since_last_exp = as.integer(day - last_exp_day, units = "days"))]
  plot <- plot(priors, data = data, n_draws = 500)
  expect_equal(length(plot$layers), 3)
})

test_that("Can plot prior predictions from model", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(4.1, 11, 65, 0.2, -0.01, 0.01,
                               2.0, 2.0, 3.0, 0.01, 0.01, 0.001)

  mod <- biokinetics$new(priors = priors,
                         data = data)
  set.seed(1)
  plot <- mod$plot_prior_predictive(tmax = 400, n_draws = 500)
  expect_equal(nrow(plot$data), 400)
  expect_equal(length(plot$layers), 7) # includes detection limits
})

test_that("Prior predictions from model are the same", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(4.1, 11, 65, 0.2, -0.01, 0.01,
                               2.0, 2.0, 3.0, 0.01, 0.01, 0.001)

  mod <- biokinetics$new(priors = priors,
                         data = data)
  set.seed(1)
  plot <- mod$plot_prior_predictive(tmax = 400, n_draws = 500)
  vdiffr::expect_doppelganger("priorpredictive", plot)
})

test_that("Can plot input data", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = data,
                         lower_censoring_limit = 40,
                         strict_lower_limit = FALSE)
  suppressWarnings({plot <- mod$plot_model_inputs()})
  vdiffr::expect_doppelganger("inputdata", plot)

  mod <- biokinetics$new(data = data,
                         lower_censoring_limit = 40,
                         covariate_formula = ~0 + infection_history,
                         strict_lower_limit = FALSE)
  suppressWarnings({plot <- mod$plot_model_inputs()})
  vdiffr::expect_doppelganger("inputdata_covariates", plot)
})

test_that("Can plot input data without detection limits", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  data[, time_since_last_exp := as.integer(day - last_exp_day, units = "days")]
  vdiffr::expect_doppelganger("inputdatanolimits", plot_sero_data(data))
})

mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

mock_model_no_covariates <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws_nocovariates.rds")))
}

mock_model_multiple_covariates <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws_multiplecovariates.rds")))
}

test_that("Can plot summarised and un-summarised population trajectories", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE)
  vdiffr::expect_doppelganger("populationtrajectories", plot(trajectories))

  unsummarised_trajectories <- mod$simulate_population_trajectories(summarise = FALSE)
  vdiffr::expect_doppelganger("populationtrajectories_unsum", plot(unsummarised_trajectories))
})

test_that("Can plot summarised and un-summarised population trajectories for multiple covariates", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model_multiple_covariates, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history + last_vax_type)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE)
  vdiffr::expect_doppelganger("multiplecovariates", plot(trajectories))

  unsummarised_trajectories <- mod$simulate_population_trajectories(summarise = FALSE)
  vdiffr::expect_doppelganger("multiplecovariates_unsum", plot(unsummarised_trajectories))
})

test_that("Can plot population trajectories with data", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = data,
                         covariate_formula = ~0 + infection_history)
  fit <- mod$fit()
  trajectories <- mod$simulate_population_trajectories()
  plot <- plot(trajectories, data = data)
  expect_equal(length(plot$scales$scales), 1)
  vdiffr::expect_doppelganger("populationtrajectories_data", plot)
})

test_that("Can plot population trajectories with log scale input data", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  data <- convert_log2_scale(data, smallest_value = min(data$value))
  mod <- biokinetics$new(data = data,
                         covariate_formula = ~0 + infection_history,
                         scale = "log")
  fit <- mod$fit()
  trajectories <- mod$simulate_population_trajectories()
  plot <- plot(trajectories, data = data)
  expect_equal(length(plot$scales$scales), 0)
  vdiffr::expect_doppelganger("populationtrajectories_logscale", plot)
})

test_that("Can plot summarised individual trajectories", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(n_draws = 250,
                                                       summarise = TRUE)
  # because these fits are so bad there are some v high upper values, so just
  # create these articially
  trajectories[, hi := ifelse(hi > 2000, me + 100, hi)]
  vdiffr::expect_doppelganger("individualtrajectories", plot(trajectories,
                                                             max_day = lubridate::ymd("2022/01/01")))
})

test_that("Can plot un-summarised individual trajectories", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(n_draws = 250,
                                                       summarise = FALSE)
  # because these fits are so bad there are some v high upper values, so just
  # truncate these
  trajectories[, mu := ifelse(mu > 2000, 2000, mu)]
  vdiffr::expect_doppelganger("individualtrajectories-unsum", plot(trajectories,
                                                                   max_day = lubridate::ymd("2022/01/01")))
})

test_that("Can plot individual trajectories for specific pids", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(n_draws = 250,
                                                       summarise = FALSE)
  # because these fits are so bad there are some v high upper values, so just
  # truncate these
  trajectories[, mu := ifelse(mu > 2000, 2000, mu)]
  vdiffr::expect_doppelganger("individualtrajectories-pids", plot(trajectories,
                                                                  pids = c("1", "2"),
                                                                  max_day = lubridate::ymd("2022/01/01")))
})

test_that("Can plot individual trajectories for specific pids with data", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(n_draws = 250,
                                                       summarise = FALSE)
  # because these fits are so bad there are some v high upper values, so just
  # truncate these
  trajectories[, mu := ifelse(mu > 2000, 2000, mu)]
  vdiffr::expect_doppelganger("individualtrajectories-pids-data", plot(trajectories,
                                                                       pids = "1",
                                                                       data = data.table::fread(system.file("delta_full.rds", package = "epikinetics")),
                                                                       max_day = lubridate::ymd("2022/01/01")))
})

test_that("Can plot individual trajectories for specific pids with data and titre type", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(n_draws = 250,
                                                       summarise = FALSE)
  # because these fits are so bad there are some v high upper values, so just
  # truncate these
  trajectories[, mu := ifelse(mu > 2000, 2000, mu)]
  vdiffr::expect_doppelganger("individualtrajectories-pids-data-alpha", plot(trajectories,
                                                                       pids = "1",
                                                                       data = data.table::fread(system.file("delta_full.rds", package = "epikinetics")),
                                                                       titre_types = "Alpha"))
})

test_that("Can plot stationary points", {
  skip_on_os("mac") # diff fails on CI for macOS
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  res <- mod$population_stationary_points()
  vdiffr::expect_doppelganger("stationarypoints", plot(res))
})

test_that("Can plot stationary points with no covariates", {
  skip_on_os("mac") # diff fails on CI for macOS
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model_no_covariates, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  res <- mod$population_stationary_points()
  vdiffr::expect_doppelganger("stationarypointsnocovariates", plot(res))
})


test_that("Can plot stationary points with upper limit", {
  skip_on_os("mac") # diff fails on CI for macOS
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model_no_covariates, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),)
  mod$fit()
  res <- mod$population_stationary_points()
  vdiffr::expect_doppelganger("stationarypointswithlimit",
                              plot(res, upper_detection_limit = 2560))
})
