mock_model_return_args <- function(name, package) {
  list(sample = function(x, ...)  list(...))
}

mock_model_multiple_covariates <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws_multiplecovariates.rds")))
}

mock_model_no_covariates <- function(name, package) {
  list(sample = function(x, ...)  readRDS(test_path("testdata", "testdraws_nocovariates.rds")))
}

test_that("Can fit model with arguments", {
  local_mocked_bindings(
    stan_package_model = mock_model_return_args, .package = "instantiate"
  )
  res <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   priors = biokinetics_priors())$fit(chains = 4,
                                            parallel_chains = 4,
                                            iter_warmup = 100,
                                            iter_sampling = 400,
                                            threads_per_chain = 4)
  expect_equal(names(res), c("chains", "parallel_chains", "iter_warmup", "iter_sampling", "threads_per_chain"))
})

test_that("Can process model fits with no covariates", {
  local_mocked_bindings(
    stan_package_model = mock_model_no_covariates, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"))

  res <- mod$fit(chains = 4,
                 parallel_chains = 4,
                 iter_warmup = 10,
                 iter_sampling = 40,
                 threads_per_chain = 4)

  pt <- mod$simulate_population_trajectories(n_draws = 100, summarise = FALSE)
  expect_equal(names(pt), c("t", ".draw", "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop",
                            "m3_pop", "mu", "titre_type"))

  pt <- mod$simulate_population_trajectories(n_draws = 100, summarise = TRUE)
  expect_equal(names(pt), c("t", "me", "lo", "hi", "titre_type"))

  it <- mod$simulate_individual_trajectories(n_draws = 100, summarise = FALSE)
  expect_equal(names(it),  c("pid", "draw", "t", "mu", "titre_type",
                             "exposure_date", "calendar_date", "time_shift"))

 # it <- mod$simulate_individual_trajectories(n_draws = 100, summarise = TRUE)
 # expect_equal(names(it), c("calendar_date", "titre_type", "me", "lo", "hi", "time_shift"))

  sp <- mod$population_stationary_points()
  expect_equal(names(sp), c("titre_type", "mu_p", "mu_s", "rel_drop_me", "mu_p_me", "mu_s_me"))
})

test_that("Can process model fits with multiple covariates", {
  local_mocked_bindings(
    stan_package_model = mock_model_multiple_covariates, .package = "instantiate"
  )
  mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                   covariate_formula = ~0 + infection_history + last_vax_type)

  res <- mod$fit(chains = 4,
                 parallel_chains = 4,
                 iter_warmup = 10,
                 iter_sampling = 40,
                 threads_per_chain = 4)

  pt <- mod$simulate_population_trajectories(n_draws = 100)
  expect_equal(names(pt), c("t", "me", "lo", "hi", "titre_type", "infection_history", "last_vax_type"))

  it <- mod$simulate_individual_trajectories(n_draws = 100)
  expect_equal(names(it), c("calendar_date", "titre_type", "me", "lo", "hi", "time_shift"))

  sp <- mod$population_stationary_points()
  expect_equal(names(sp), c("infection_history", "last_vax_type", "titre_type",
                            "mu_p", "mu_s", "rel_drop_me", "mu_p_me", "mu_s_me"))
})
