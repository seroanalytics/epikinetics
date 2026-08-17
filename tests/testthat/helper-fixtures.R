example_epikinetics_data <- function() {
  data.frame(
    pid = rep(c("P-01", "P-02", "P-03"), each = 6),
    day = rep(as.Date("2024-01-01") + c(0, 7, 30), each = 2, times = 3),
    last_exp_day = rep(
      as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
      each = 6
    ),
    titre_type = rep(c("A", "B"), 9),
    value = c(
      5, 10, 40, 80, 20, 30,
      6, 12, 50, 100, 25, 35,
      7, 14, 60, 120, 30, 40
    ),
    group = rep(c("reference", "treated", "treated"), each = 6),
    age = rep(c(30, 40, 50), each = 6),
    stringsAsFactors = FALSE
  )
}

fake_epikinetics_fit <- function(prepared = NULL, ndraws = 20L) {
  if (is.null(prepared)) {
    prepared <- prepare_epikinetics_data(
      example_epikinetics_data(),
      formula = ~ group,
      lower_limit = c(A = 5, B = 10),
      upper_limit = c(A = 1000, B = 1000)
    )
  }
  parameters <- c(
    "baseline", "time_to_peak", "waning_duration", "boost_rate",
    "early_waning_rate", "late_waning_rate"
  )
  defaults <- c(
    baseline = 6,
    time_to_peak = 10,
    waning_duration = 50,
    boost_rate = 0.25,
    early_waning_rate = 0.02,
    late_waning_rate = 0.002
  )
  values <- list()
  draw_shift <- seq(-0.1, 0.1, length.out = ndraws)

  for (parameter in parameters) {
    for (k in seq_len(prepared$stan_data$N_biomarkers)) {
      variable <- paste0("population_", parameter, "[", k, "]")
      values[[variable]] <- defaults[[parameter]] *
        if (parameter == "baseline") 1 else exp(draw_shift / 10)
      if (parameter == "baseline") {
        values[[variable]] <- defaults[[parameter]] + draw_shift
      }

      sd_variable <- paste0("participant_sd_", parameter, "[", k, "]")
      values[[sd_variable]] <- rep(0.1, ndraws)
    }
    active_participant <- prepared$mappings$participant_parameters
    if (is.null(active_participant)) active_participant <- parameters
    if (parameter %in% active_participant) {
      for (i in seq_len(prepared$stan_data$N_participants)) {
        values[[paste0("z_", parameter, "[", i, "]")]] <-
          rep((i - 1) / 10, ndraws)
      }
    }
    for (p in seq_len(prepared$stan_data$N_covariates)) {
      values[[paste0("beta_", parameter, "[", p, "]")]] <-
        rep(0.05 * p, ndraws)
    }
  }
  values$observation_sd <- rep(0.5, ndraws)
  draws <- posterior::as_draws_matrix(do.call(cbind, values))
  structure(
    list(
      fit = draws,
      prepared = prepared,
      computation = list(
        chains = 1L,
        parallel_chains = 1L,
        threads_per_chain = 1L,
        grainsize = prepared$stan_data$N_participants
      ),
      model = list(package_version = "test", cmdstan_version = "test")
    ),
    class = "epikinetics_fit"
  )
}
