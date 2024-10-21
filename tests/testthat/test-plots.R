test_that("Can plot prior prediction up to tmax", {
  priors <- biokinetics_priors()
  plot <- plot_prior_predictive(priors, tmax = 100, n_draws = 500)
  expect_equal(nrow(plot$data), 100)
  expect_equal(length(plot$layers), 2)
})

test_that("Can plot prior prediction with data points", {
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors()
  expect_error(plot_prior_predictive(priors, data = data), "Missing required columns: t_since_last_exp")
  data[, `:=`(t_since_last_exp = as.integer(day - last_exp_day, units = "days"))]
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
  skip_on_ci()
  data <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
  priors <- biokinetics_priors(mu_values = c(4.1, 11, 65, 0.2, -0.01, 0.01),
                               sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.001))

  mod <- biokinetics$new(priors = priors,
                         data = data)
  set.seed(1)
  plot <- mod$plot_prior_predictive(tmax = 400, n_draws = 500)
  vdiffr::expect_doppelganger("priorpredictive", plot)
})
