simulate_trajectory <- function(t, t0, tp, ts, m1, m2, m3) {

  mu <-  t0

  if (t < tp) {
    mu <- mu + m1*t
  } else if(t <= ts)  {
    mu <- mu + m1*tp+ m2*(t - tp)
  } else if (t > ts) {
    mu <- mu + m1*tp + m2*(ts - tp) + m3*(t - ts)
  }

  mu = max(mu, 0)
  return(mu)
}

simulate_trajectories <- function(
  dt_samples,
  time_range = seq(0, 120),
  by_manual = c("t", "p", "k", ".draw"),
  ind) {

  dt_samples_proc <- copy(dt_samples)
  dt_times = data.table(t = time_range)

  # adding artificial ids so that we can do a big merge, adding times to
  # each set of parameter samples
  dt_times[, t_id := 1, by = t]
  dt_samples_proc[, t_id := 1]

  dt_trajectories <- merge(
    dt_samples_proc, dt_times,
    by = "t_id",
    allow.cartesian = TRUE
  )

  dt_trajectories[, mu := simulate_trajectory(
    t, t0, tp, ts, m1, m2, m3),
                    by = by_manual]

  #--- DEPRECATED, USING DIFFERENT COLUMN NAMES FOR POPULATION AND
  #--- INDIVIDUAL-LEVEL PRIORS. HAVE CHANGED TO FLEXIBLE PARAMETER NAMES
  #--- NEED TO CHANGE EVERYWHERE NOW
  # if(ind == FALSE) {
  #   dt_trajectories[, mu := simulate_trajectory(
  #     t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
  #     by = by_manual]
  # } else if (ind == TRUE) {
  #   dt_trajectories[, mu := simulate_trajectory(
  #     t, t0_ind, tp_ind, ts_ind, m1_ind, m2_ind, m3_ind),
  #     by = by_manual]
  #   }

  return(dt_trajectories)
}

extract_parameters_ind <- function(
  fit, params = c(
    "t0_ind[n, k]", "tp_ind[n, k]", "ts_ind[n, k]",
    "m1_ind[n, k]", "m2_ind[n, k]", "m3_ind[n, k]"),
  format = "wide",
  add_variation_params = TRUE) {

  if(add_variation_params == TRUE) {

    ind_var_params <- c(
      "z_t0[n]", "z_tp[n]", "z_ts[n]", "z_m1[n]", "z_m2[n]", "z_m3[n]")

    params = c(params, ind_var_params)
  }

  params_proc <- rlang::parse_exprs(params)

  dt_out <- tidybayes::spread_draws(fit, !!!params_proc) |>
    data.table()

  dt_out[, `:=` (.chain = NULL, .iteration = NULL)]

  setcolorder(dt_out, c("n", "k", ".draw"))
  setnames(dt_out, c("n", "k", ".draw"), c("stan_id", "titre_type", "draw"))

  return(dt_out)
}

simulate_trajectories_ind <- function(
  fit, dt_data, n_draws, wave_manual,
  scale = "log", adjust_dates = FALSE,
  time_shift, t_max = 150) {

  # Extracting parameters from fit
  dt_params_ind <- extract_parameters_ind(
    fit, add_variation_params = FALSE)[!is.nan(t0_ind)]

  # Calculating the maximum time each individual has data for after the
  # exposure of interest
  dt_max_dates <- dt_data[
    , .(t_max = max(t_since_last_exp)), by = .(stan_id)]

  # A very small number of individuals have bleeds on the same day or a few days
  # after their recorded exposure dates, resulting in very short trajectories.
  # Adding a 50 day buffer to any individuals with less than or equal to 50 days
  # of observations after their focal exposure
  dt_max_dates <- dt_max_dates[t_max <= 50, t_max := 50, by = .(stan_id)]
  # dt_max_dates <- dt_max_dates[t_max > 50, t_max := t_max, by = .(stan_id)]

  # Merging the parameter draws with the maximum time data.table
  dt_params_ind <- merge(dt_params_ind, dt_max_dates, by = "stan_id")

  dt_params_ind_trim <- dt_params_ind[, .SD[draw %in% 1:n_draws], by = stan_id]

  # Running the C++ code to simulate trajectories for each parameter sample
  # for each individual
  dt_params_ind_traj <- simulation_wrapper_cpp(dt_params_ind_trim) |>
    data.table(Wave = wave_manual)

  if(scale == "natural") {
    # Converting back to the original scale
    dt_params_ind_traj <- convert_log_scale_inverse_cpp(
      dt_params_ind_traj, vars_to_transform = "mu")
  }

  dt_titre_types <- data.table(
    titre_type = dt_data[, unique(titre_type)],
    titre_type_num = dt_params_ind_traj[, unique(titre_type_num)])

  dt_params_ind_traj <- merge(
    dt_params_ind_traj,
    dt_titre_types,
    by = "titre_type_num")[, titre_type_num := NULL]

  if(adjust_dates == FALSE) {
    dt_lookup <- dt_data[, .(
      exposure_date = min(last_exp_date)),
                           by = .(id, stan_id, infection_history)]
  } else if(adjust_dates == TRUE) {
    dt_lookup <- dt_data[, .(
      exposure_date = min(last_exp_date) - time_shift),
                           by = .(id, stan_id, infection_history)]
  }

  dt_out <- merge(dt_params_ind_traj, dt_lookup, by = "stan_id")

  dt_out[
    , calendar_date := exposure_date + t,
      by = .(stan_id, titre_type, infection_history)]

  return(dt_out)
}

simulate_ind_traj_wrapper <- function(
  fit_1, data_1, fit_2, data_2, fit_3, data_3,
  wave_1 = "Delta", wave_2 = "BA.2", wave_3 = "XBB",
  adjust_dates, n_draws,
  time_shift) {

  if(adjust_dates == TRUE) {
    dt_trajectories_1 <- simulate_trajectories_ind(
      fit_1, data_1, n_draws = n_draws,
      wave_manual = wave_1, scale = "log",
      adjust_dates = TRUE,
      time_shift = time_shift)

    dt_trajectories_2 <- simulate_trajectories_ind(
      fit_2, data_2, n_draws = n_draws,
      wave_manual = wave_2, scale = "log",
      adjust_dates = TRUE,
      time_shift = time_shift)

    dt_trajectories_3 <- simulate_trajectories_ind(
      fit_3, data_3, n_draws = n_draws,
      wave_manual = wave_3, scale = "log",
      adjust_dates = TRUE,
      time_shift = time_shift)
  } else if (adjust_dates == FALSE) {
    dt_trajectories_1 <- simulate_trajectories_ind(
      fit_1, data_1, n_draws = n_draws,
      wave_manual = wave_1,
      scale = "log", adjust_dates = FALSE)

    dt_trajectories_2 <- simulate_trajectories_ind(
      fit_2, data_2, n_draws = n_draws,
      wave_manual = wave_2,
      scale = "log", adjust_dates = FALSE)

    dt_trajectories_3 <- simulate_trajectories_ind(
      fit_3, data_3, n_draws = n_draws,
      wave_manual = wave_3,
      scale = "log", adjust_dates = FALSE)
  }

  dt_trajectories <- rbind(
    dt_trajectories_1,
    dt_trajectories_2,
    dt_trajectories_3)

  dt_trajectories <- dt_trajectories[
    , Wave := fct_relevel(Wave, c(wave_1, wave_2, wave_3))]

  return(dt_trajectories)
}

simulate_and_sum_pop_mean_from_ind <- function(
  fit_1, data_1, fit_2, data_2, fit_3, data_3,
  wave_1 = "Delta", wave_2 = "BA.2", wave_3 = "XBB",
  adjust_dates, n_draws, time_shift, formula) {

  dt_trajectories <- simulate_ind_traj_wrapper(
    fit_1 = fit_1, data_1 = data_1,
    fit_2 = fit_2, data_2 = data_2,
    fit_3 = fit_3, data_3 = data_3,
    wave_1 = wave_1, wave_2 = wave_2, wave_3 = wave_3,
    adjust_dates = adjust_dates, n_draws = n_draws,
    time_shift = time_shift)

  dt_trajectories <- clean_covariate_names(
    dt_trajectories,
    formula_val = formula,
    c("Infection history", "Titre type"))

  dt_trajectories_mean <- dt_trajectories[
    !is.nan(mu), .(pop_mu_sum = mean(mosaic::resample(mu))),
    by = .(calendar_date, draw, Wave, `Titre type`)]

  dt_trajectories_mean_sum <- summarise_draws(
    dt_trajectories_mean,
    column_name = "pop_mu_sum",
    by = c("calendar_date", "Wave", "Titre type"))

  convert_log_scale_inverse(dt_trajectories_mean_sum)

  return(dt_trajectories_mean_sum)
}

simulate_and_sum_ind <- function(
  fit, dt_data, n_draws, wave_manual,
  scale = "log", adjust_dates = FALSE,
  time_shift, t_max = 150, formula) {

  dt_trajectories <- simulate_trajectories_ind(
    fit = fit, dt_data = dt_data,
    n_draws = n_draws, wave_manual = wave_manual,
    scale = scale, adjust_dates = adjust_dates,
    time_shift = time_shift, t_max = t_max)

  dt_trajectories <- clean_covariate_names(
    dt_trajectories,
    formula_val = formula,
    c("Infection history", "Titre type"))

  dt_trajectories_sum <- summarise_draws(
    dt_trajectories,
    column_name = "mu",
    by = c(
      "stan_id", "id", "Infection history",
      "calendar_date", "Wave", "Titre type"))

  convert_log_scale_inverse(dt_trajectories_sum)

  return(dt_trajectories_sum)
}
