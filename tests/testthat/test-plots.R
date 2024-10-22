test_that("Can plot prior prediction up to tmax", {
  priors <- biokinetics_priors()
  plot <- plot_prior_predictive(priors, tmax = 100, n_draws = 500)
  expect_equal(nrow(plot$data), 100)
  expect_equal(length(plot$layers), 2)
})

test_that("Can plot prior prediction with data points", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors()
  expect_error(plot_prior_predictive(priors, data = data), "Missing required columns: time_since_last_exp")
  data[, `:=`(time_since_last_exp = as.integer(day - last_exp_day, units = "days"))]
  plot <- plot_prior_predictive(priors, data = data, n_draws = 500)
  expect_equal(length(plot$layers), 3)
})

test_that("Can plot prior predictions from model", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(mu_values = c(4.1, 11, 65, 0.2, -0.01, 0.01),
                               sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.001))

  mod <- biokinetics$new(priors = priors,
                         data = data)
  set.seed(1)
  plot <- mod$plot_prior_predictive(tmax = 400, n_draws = 500)
  expect_equal(nrow(plot$data), 400)
  expect_equal(length(plot$layers), 3)
})

test_that("Prior predictions from model are the same", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(mu_values = c(4.1, 11, 65, 0.2, -0.01, 0.01),
                               sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.001))

  mod <- biokinetics$new(priors = priors,
                         data = data)
  set.seed(1)
  plot <- mod$plot_prior_predictive(tmax = 400, n_draws = 500)
  vdiffr::expect_doppelganger("priorpredictive", plot)
})

test_that("Can plot input data", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = data)
  plot <- mod$plot_model_inputs()
  vdiffr::expect_doppelganger("inputdata", plot)

  mod <- biokinetics$new(data = data, covariate_formula = ~0 + infection_history)
  plot <- mod$plot_model_inputs()
  vdiffr::expect_doppelganger("inputdata_covariates", plot)
})

mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

test_that("Summarised and un-summarised population trajectories give same plots", {
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_population_trajectories(summarise = TRUE)
  unsummarised_trajectories <- mod$simulate_population_trajectories(summarise = FALSE)
  vdiffr::expect_doppelganger("populationtrajectories", plot(trajectories))
  vdiffr::expect_doppelganger("populationtrajectories", plot(unsummarised_trajectories))
})

test_that("Can plot population trajectories with data", {
  local_mocked_bindings(
    stan_package_model = mock_model, .package = "instantiate"
  )
  # note that this is using a pre-fitted model with very few iterations, so the
  # fits won't look very good
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  mod <- biokinetics$new(data = data, covariate_formula = ~0 + infection_history)
  fit <- mod$fit()
  trajectories <- mod$simulate_population_trajectories()
  vdiffr::expect_doppelganger("populationtrajectories_data", plot(trajectories, data = data))
})
