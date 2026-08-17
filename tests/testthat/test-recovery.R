test_that("the model recovers a simulated population trajectory", {
  skip_if_not(
    identical(Sys.getenv("EPIKINETICS_RUN_RECOVERY_TESTS"), "true"),
    "set EPIKINETICS_RUN_RECOVERY_TESTS=true for the recovery test"
  )
  skip_if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))

  set.seed(8241)
  participants <- sprintf("P-%02d", seq_len(24))
  times <- c(0, 7, 14, 30, 60, 100, 150, 220)
  grid <- expand.grid(
    pid = participants,
    time = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  participant_index <- match(grid$pid, participants)
  z <- replicate(4, rnorm(length(participants)))
  baseline <- 6 + 0.4 * z[, 1]
  time_to_peak <- rep(10, length(participants))
  duration <- rep(50, length(participants))
  boost <- 0.25 * exp(0.10 * z[, 2])
  early <- 0.02 * exp(0.12 * z[, 3])
  late <- 0.002 * exp(0.15 * z[, 4])
  mean <- epikinetics:::kinetics_mean(
    grid$time,
    baseline[participant_index],
    time_to_peak[participant_index],
    time_to_peak[participant_index] + duration[participant_index],
    boost[participant_index],
    early[participant_index],
    late[participant_index]
  )
  grid$day <- as.Date("2024-01-01") + grid$time
  grid$last_exp_day <- as.Date("2024-01-01")
  grid$titre_type <- "simulated"
  grid$value <- 2^(mean + rnorm(nrow(grid), sd = 0.4))

  prepared <- prepare_epikinetics_data(grid)
  fit <- fit_epikinetics(
    prepared,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 2,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 8241,
    refresh = 0
  )
  diagnostics <- diagnose_epikinetics(fit, quiet = TRUE)
  expect_equal(diagnostics$overview$divergences, 0)
  expect_equal(diagnostics$overview$chains_with_nonfinite_ebfmi, 0)

  fitted <- predict(fit, times = times, scale = "model")
  truth <- epikinetics:::kinetics_mean(
    times,
    baseline = rep(6, length(times)),
    time_to_peak = rep(10, length(times)),
    waning_change_time = rep(60, length(times)),
    boost_rate = rep(0.25, length(times)),
    early_waning_rate = rep(0.02, length(times)),
    late_waning_rate = rep(0.002, length(times))
  )
  covered <- truth >= fitted$lower & truth <= fitted$upper
  # This is one finite simulated dataset, not a coverage-calibration study.
  # Require the known curve to be covered at most grid points and use RMSE as
  # the complementary whole-trajectory recovery criterion.
  expect_gte(sum(covered), length(times) - 2L)
  expect_lt(sqrt(mean((fitted$estimate - truth)^2)), 0.5)
})
