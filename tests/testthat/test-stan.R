stan_function_text <- function(path, function_name) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(paste0("real ", function_name, "("), lines, fixed = TRUE)[1L]
  expect_false(is.na(start))
  depth <- 0L
  opened <- FALSE
  selected <- character()
  for (line in lines[start:length(lines)]) {
    selected <- c(selected, line)
    openings <- lengths(regmatches(line, gregexpr("{", line, fixed = TRUE)))
    closings <- lengths(regmatches(line, gregexpr("}", line, fixed = TRUE)))
    if (openings) opened <- TRUE
    depth <- depth + openings - closings
    if (opened && depth == 0L) break
  }
  gsub(
    "[[:space:]]+",
    " ",
    paste(trimws(selected), collapse = " ")
  )
}

test_that("Stan source uses participant-grouped threaded likelihood", {
  source_path <- epikinetics:::epikinetics_stan_file()
  source <- readLines(source_path, warn = FALSE)
  expect_true(any(grepl("reduce_sum", source, fixed = TRUE)))
  expect_true(any(grepl("participant_partial_sum", source, fixed = TRUE)))
  expect_true(any(grepl("observation_start", source, fixed = TRUE)))
  expect_true(any(grepl("population_waning_duration", source, fixed = TRUE)))
  expect_true(any(grepl("right_censored_normal_lpdf", source, fixed = TRUE)))
  expect_true(any(grepl("positive_normal_from_raw", source, fixed = TRUE)))
  expect_true(any(grepl("half_normal_from_raw", source, fixed = TRUE)))
  expect_true(any(grepl("covariate_active", source, fixed = TRUE)))
  expect_true(any(grepl("participant_effect_active", source, fixed = TRUE)))
  expect_true(any(grepl("inv_erfc", source, fixed = TRUE)))
  expect_false(any(grepl("lp += normal_lccdf", source, fixed = TRUE)))
  expect_identical(
    stan_function_text(source_path, "kinetics_mean"),
    stan_function_text(
      testthat::test_path("stan", "kinetics-reference.stan"),
      "kinetics_mean"
    )
  )
})

test_that("a small threaded Stan fit runs end-to-end", {
  skip_if_not(identical(Sys.getenv("EPIKINETICS_RUN_STAN_TESTS"), "true"))
  skip_if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))

  set.seed(7301)
  participants <- sprintf("P-%02d", seq_len(12))
  times <- c(0, 7, 14, 30, 60, 100, 160, 220)
  data <- expand.grid(
    pid = participants,
    day = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  participant <- match(data$pid, participants)
  z <- replicate(4, stats::rnorm(length(participants)))
  baseline <- 4.2 + 0.25 * z[, 1L]
  boost <- 0.25 * exp(0.12 * z[, 2L])
  early <- 0.02 * exp(0.12 * z[, 3L])
  late <- 0.002 * exp(0.16 * z[, 4L])
  expected <- epikinetics:::kinetics_mean(
    data$day,
    baseline[participant],
    rep(10, nrow(data)),
    rep(60, nrow(data)),
    boost[participant],
    early[participant],
    late[participant]
  )
  measured <- 2^(expected + stats::rnorm(nrow(data), sd = 0.35))
  data$last_exp_day <- 0
  data$titre_type <- "simulated"
  data$value <- pmin(pmax(measured, 16), 90)

  model_data <- prepare_epikinetics_data(
    data,
    lower_limit = 16,
    upper_limit = 90
  )
  expect_equal(
    stan_data(model_data)$participant_effect_active,
    c(1L, 0L, 0L, 1L, 1L, 1L)
  )
  expect_true(any(stan_data(model_data)$censoring == -1L))
  expect_true(any(stan_data(model_data)$censoring == 1L))
  compiled <- compile_epikinetics_model()
  expect_true(isTRUE(compiled$cpp_options()$stan_threads))

  fit <- fit_epikinetics(
    model_data,
    chains = 2,
    parallel_chains = 2,
    threads_per_chain = 2,
    iter_warmup = 250,
    iter_sampling = 50,
    adapt_delta = 0.95,
    refresh = 0,
    seed = 123
  )
  expect_s3_class(cmdstan_fit(fit), "CmdStanMCMC")
  expect_equal(fit$computation$sampling_state, "complete")
  expect_true(all(cmdstan_fit(fit)$return_codes() == 0L))
  expect_equal(fit$computation$threads_per_chain, 2L)
  sampler <- cmdstan_fit(fit)$diagnostic_summary(quiet = TRUE)
  expect_equal(sum(sampler$num_divergent), 0L)
  expect_equal(sum(sampler$num_max_treedepth), 0L)
  expect_true(all(is.finite(sampler$ebfmi)))
  # This smoke fit deliberately has too few draws for stable ESS estimates.
  diagnostics <- suppressWarnings(diagnose_epikinetics(fit, quiet = TRUE))
  expect_equal(diagnostics$overview$divergences, 0L)
  parameters <- posterior_parameters(fit)
  expect_gt(nrow(parameters), 0)
  expect_true(all(is.finite(parameters$mean)))
  population <- predict(fit, times = c(0, 10, 60), ndraws = 50)
  expect_true(all(is.finite(population$median)))
  expect_true(all(population$upper > population$lower))
  expect_s3_class(plot(population), "ggplot")
  individual <- suppressWarnings(
    predict(
      fit,
      type = "individual",
      participants = "P-01",
      times = c(0, 10),
      ndraws = 10
    )
  )
  expect_true(all(individual$upper > individual$lower))
})

test_that("R and Stan kinetic functions agree numerically", {
  skip_if_not(identical(Sys.getenv("EPIKINETICS_RUN_STAN_TESTS"), "true"))
  skip_if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))

  time <- c(0, 7, 10, 10.5, 60, 61, 150)
  data <- list(
    N = length(time),
    time = time,
    baseline = 6.2,
    time_to_peak = 10,
    waning_change_time = 60,
    boost_rate = 0.23,
    early_waning_rate = 0.021,
    late_waning_rate = 0.0023
  )
  stan_file <- tempfile("epikinetics-kinetics-reference-", fileext = ".stan")
  expect_true(file.copy(
    testthat::test_path("stan", "kinetics-reference.stan"),
    stan_file
  ))
  model <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)
  fit <- model$sample(
    data = data,
    chains = 1,
    parallel_chains = 1,
    iter_warmup = 0,
    iter_sampling = 1,
    fixed_param = TRUE,
    refresh = 0,
    seed = 419
  )
  stan <- as.numeric(
    posterior::as_draws_matrix(fit$draws("expected_value"))[1, ]
  )
  r <- epikinetics:::kinetics_mean(
    time,
    baseline = rep(data$baseline, length(time)),
    time_to_peak = rep(data$time_to_peak, length(time)),
    waning_change_time = rep(data$waning_change_time, length(time)),
    boost_rate = rep(data$boost_rate, length(time)),
    early_waning_rate = rep(data$early_waning_rate, length(time)),
    late_waning_rate = rep(data$late_waning_rate, length(time))
  )
  expect_equal(r, stan, tolerance = 1e-10)
})
