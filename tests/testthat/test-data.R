test_that("Can construct stan data", {
  dt <- data.table::fread(system.file("delta_trunc.rds", package = "epikinetics"))
  stan_dt <- prepare_stan_data(data = dt,
                               priors = epikinetics_priors(),
                               covariate_formula = ~ 0 + infection_history,
                               preds_sd = 0.25,
                               time_type = "relative")
  expect_equal(stan_dt$N, 1220)
  expect_equal(stan_dt$N_events, 286)
  expect_equal(unlist(stan_dt$id), unname(unlist(dt[, "stan_id"])))
})
