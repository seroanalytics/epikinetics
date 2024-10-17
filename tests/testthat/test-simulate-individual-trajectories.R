test_that("Cpp and R function produce same values", {
  t_max <- 6L
  dat_pop <- data.table(t = 0:t_max,
                        t0 = 0,
                        tp = 3,
                        ts = 5,
                        m1 = 0.5,
                        m2 = 1,
                        m3 = 1.5
  )
  res_pop <- dat_pop[, mu := biokinetics_simulate_trajectory(t, t0, tp, ts, m1, m2, m3), by = "t"]
  dat_ind <- data.table(pid = 1L,
                        t_max = t_max,
                        k = 3L,
                        draw = 10L,
                        t0_ind = 0,
                        tp_ind = 3,
                        ts_ind = 5,
                        m1_ind = 0.5,
                        m2_ind = 1,
                        m3_ind = 1.5
  )

  res_ind <- biokinetics_simulate_trajectories(dat_ind)
  expect_equal(res_pop$mu, res_ind$mu)
})

mock_model <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws.rds")))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Cannot retrieve trajectories until model is fitted", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"))
  expect_error(mod$simulate_individual_trajectories(), "Model has not been fitted yet. Call 'fit' before calling this function.")
})

test_that("Validates inputs", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  expect_error(mod$simulate_individual_trajectories(summarise = "bad"), "'summarise' must be logical")
  expect_error(mod$simulate_individual_trajectories(n_draws = "bad"), "'n_draws' must be numeric")
  expect_error(mod$simulate_individual_trajectories(time_shift = "bad"), "'time_shift' must be numeric")
})

test_that("Can retrieve summarised trajectories", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = TRUE, n_draws = 10)
  expect_equal(names(trajectories), c("calendar_day", "titre_type", "me", "lo", "hi", "time_shift"))
})

test_that("Can retrieve un-summarised trajectories", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE, n_draws = 10)
  expect_equal(names(trajectories), c("pid", "draw", "time_since_last_exp", "mu", "titre_type", "infection_history",
                                      "exposure_day", "calendar_day", "time_shift"))
})

test_that("Only n_draws draws are returned", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE, n_draws = 10)
  expect_true(all(trajectories$draw <= 10))
})

test_that("Exposure dates are brought forward by time_shift days", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history)
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = FALSE, n_draws = 10)
  trajectories_shifted <- mod$simulate_individual_trajectories(summarise = FALSE, n_draws = 10, time_shift = 75)
  expect_equal(trajectories$mu, trajectories_shifted$mu)
  expect_true(all(as.numeric(difftime(trajectories$exposure_date, trajectories_shifted$exposure_date, units = "days")) == 75))
})

test_that("Natural scale data is returned on natural scale", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history, scale = "natural")
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = TRUE, n_draws = 10)
  expect_false(all(trajectories$me < 10))
})

test_that("Log scale data is returned on log scale", {
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                         covariate_formula = ~0 + infection_history, scale = "log")
  mod$fit()
  trajectories <- mod$simulate_individual_trajectories(summarise = TRUE, n_draws = 10)
  expect_true(all(trajectories$me < 10))
})
