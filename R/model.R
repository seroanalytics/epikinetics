#' @title Fit the model.
#' @export
#' @description Fit the kinetics model and return fitted model.
#' @return A CmdStanMCMC fitted model object.
#' @param data Optional data table of model inputs. One of data or file must be provided.
#' @param file_path Optional file path to model inputs in CSV format. One of data or file must be provided.
#' @param priors Object of type 'epikinetic_priors'.
#' @param covariate_formula Formula specifying hierarchical structure of model. Default ~0 + infection_history.
#' @param preds_sd Standard deviation of predictor coefficients. Default 0.25.
#' @param time_type One of 'relative' or 'absolute'. Default 'relative'.
#' @param ... Named arguments to the `sample()` method of CmdStan model.
#'   objects: <https://mc-stan.org/cmdstanr/reference/model-method-sample.html>
run_model <- function(priors,
                      data = NULL,
                      file_path = NULL,
                      covariate_formula = ~0 + infection_history,
                      preds_sd = 0.25,
                      time_type = "relative",
                      ...) {
  if (is.null(data) && is.null(file_path)) {
    stop("One of 'data' or 'file_path' must be provided")
  }
  if (!is.null(data) && !is.null(file_path)) {
    stop("Only one of 'data' or 'file_path' should be provided")
  }
  if (!inherits(priors, "epikinetics_priors")) {
    stop("'priors' must be of type 'epikinetics_priors'")
  }
  if (!is.numeric(preds_sd)) {
    stop("'preds_sd' must be a number")
  }
  if (!(time_type %in% c("relative", "absolute"))) {
    stop("'time_type' must be one of 'relative' or 'absolute'")
  }
  if (is.null(data)) {
    data <- data.table::fread(file_path)
  }
  logger::log_info("Preparing stan data")
  stan_data <- prepare_stan_data(data = data,
                                 covariate_formula = covariate_formula,
                                 priors = priors,
                                 time_type = time_type,
                                 preds_sd = preds_sd)
  logger::log_info("Retrieving compiled model")
  model <- instantiate::stan_package_model(
    name = "antibody_kinetics_main",
    package = "epikinetics"
  )
  logger::log_info("Fitting model")
  model$sample(stan_data, ...)
}

#' @title Prepare data for stan model.
#' @description Shape the data to named list format for stan.
#' @return A named list.
#' @param data Data table of model inputs.
#' @param priors Object of type 'epikinetic_priors'.
#' @param covariate_formula Formula specifying hierarchical structure of model.
#' @param preds_sd Standard deviation of predictor coefficients.
#' @param time_type One of 'relative' or 'absolute'.
prepare_stan_data <- function(
  data,
  priors,
  covariate_formula,
  preds_sd,
  time_type) {

  stan_id <- titre <- censored <- titre_type_num <- titre_type <- obs_id <- t_since_last_exp <- t_since_min_date <- NULL
  stan_data <- list(
    N = data[, .N],
    # N_ind = data[, data.table::uniqueN(id)],
    N_events = data[, data.table::uniqueN(stan_id)],
    id = data[, stan_id],
    titre = data[, titre],
    censored = data[, censored],
    titre_type = data[, titre_type_num],
    preds_sd = preds_sd,
    K = data[, data.table::uniqueN(titre_type)],
    N_uncens = data[censored == 0, .N],
    N_lo = data[censored == -2, .N],
    N_me = data[censored == -1, .N],
    N_hi = data[censored == 1, .N],
    uncens_idx = data[censored == 0, obs_id],
    cens_lo_idx = data[censored == -2, obs_id],
    cens_me_idx = data[censored == -1, obs_id],
    cens_hi_idx = data[censored == 1, obs_id])

  if (time_type == "relative") {
    stan_data$t <- data[, t_since_last_exp]
  } else if (time_type == "absolute") {
    stan_data$t <- data[, t_since_min_date]
  }

  X <- construct_design_matrix(data = data,
                               covariate_formula = covariate_formula)
  stan_data$X <- X
  stan_data$P <- ncol(X)
  # stan_data$formula <- formula

  stan_data <- c(stan_data, priors)
  return(stan_data)
}

construct_design_matrix <- function(data, covariate_formula) {
  var <- stan_id <- NULL
  all_formula_variables <- all.vars(covariate_formula)
  dt_design_matrix <- data[, .SD, .SDcols = all_formula_variables, by = stan_id] |>
    unique()

  # Build the full design matrix using model.matrix
  mm <- model_matrix_with_dummy(data = dt_design_matrix,
                                covariate_formula = covariate_formula)

  # Identify columns with no variance and remove them
  variance_per_column <- apply(mm, 2, var)
  relevant_columns <- which(variance_per_column != 0)
  mm_reduced <- mm[, relevant_columns]

  return(mm_reduced)
}

model_matrix_with_dummy <- function(data, covariate_formula) {

  # Identify columns that are factors with one level
  single_level_factors <- sapply(data, function(col) {
    is.factor(col) && length(levels(col)) == 1
  })

  # If any such columns are found, add a dummy level and row
  if (any(single_level_factors)) {
    dummy_row <- data[1, , drop = FALSE] # Create a dummy row based on the first row of data
    for (colname in names(single_level_factors)[single_level_factors]) {
      dummy_level <- paste0(levels(data[[colname]])[1], "_dummy")
      levels(data[[colname]]) <- c(levels(data[[colname]]), dummy_level)
      dummy_row[[colname]] <- dummy_level
    }
    data <- rbind(data, dummy_row) # Append the dummy row to data
  }

  # Compute the model matrix
  mm <- stats::model.matrix(covariate_formula, data)

  # If dummy row was added, remove the corresponding row from the model matrix
  if (any(single_level_factors)) {
    mm <- mm[-nrow(mm), , drop = FALSE]
  }

  return(mm)
}

#' @title Process model fits.
#' @export
#' @description Process the stan model results into a data table.
#' @return A Table.
#' @param fits A CmdStanMCMC fitted model object.
#' @param data Data table. The original data used to fit the model.
#' @param covariate_formula Formula specifying hierarchical structure of model. Ddefault  ~0 + infection_history.
#' @param time_type One of 'relative' or 'absolute'. Default 'relative'.
#' @param t_max Numeric
#' @param summarise Boolean
#' @param by Vector of covariates to summarise by. Can be any of 'titre_type' plus covariates specified
#' in covariate_formula. Default c("infection_history", "titre_type").
#' @param scale One of 'natural' or 'log'. Default 'natural'.
#' @param n_draws Integer. Number of samples to draw. Default 2500.
process_fits <- function(
  fits,
  data,
  covariate_formula = ~0 + infection_history,
  time_type = "relative",
  t_max = 150,
  summarise = TRUE,
  by = c("infection_history", "titre_type"),
  scale = "natural",
  n_draws = 2500) {

  dt_sum <- summarise_pop_fit(
    fits, time_range = seq(0, t_max), summarise = summarise,
    n_draws = n_draws)

  dt_out <- recover_covariate_names(
    dt_sum, data, covariate_formula)

  if (time_type == "absolute") {
    dt_out[, date := dt[, unique(min(date))] + t,
             by = by]
  }

  dt_out <- dt_out[
    , lapply(.SD, function(x) if (is.factor(x)) forcats::fct_drop(x) else x)]

  if (scale == "natural" & summarise == FALSE) {
    dt_out <- convert_log_scale_inverse(
      dt_out, vars_to_transform = "mu")
  } else if (scale == "natural" & summarise == TRUE) {
    dt_out <- convert_log_scale_inverse(
      dt_out, vars_to_transform = c("me", "lo", "hi"))
  }

  return(dt_out)
}


convert_log_scale_inverse <- function(
  dt_in, vars_to_transform = c("me", "lo", "hi")) {

  for(var in vars_to_transform) {
    # # Reverse the log2 transformation and multiplication by 5.
    # dt_in[, (var) := ifelse(
    #   get(var) <= 1, 5*2^(get(var)), 5*2^(get(var) + 1))]
    dt_in[, (var) := 5*2^(get(var))]

  }
  return(dt_in)
}


summarise_pop_fit <- function(
  fit,
  time_range = seq(0, 200, 1),
  summarise = TRUE,
  n_draws = 2500) {

  # Declare variables to suppress notes when compiling package
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  t0_pop <- tp_pop <- ts_pop <- m1_pop <- m2_pop <- m3_pop <- NULL
  beta_t0 <- beta_tp <- beta_ts <- beta_m1 <- beta_m2 <- beta_m3 <- NULL
  k <- p <- .draw <- t_id <- mu <- NULL

  dt_samples_wide <- tidybayes::spread_draws(
    fit,
    t0_pop[k], tp_pop[k], ts_pop[k],
    m1_pop[k], m2_pop[k], m3_pop[k],
    beta_t0[p], beta_tp[p], beta_ts[p],
    beta_m1[p], beta_m2[p], beta_m3[p]) |>
    data.table()

  dt_samples_wide <- dt_samples_wide[.draw %in% 1:n_draws]

  dt_samples_wide[, `:=`(.chain = NULL, .iteration = NULL)]

  data.table::setcolorder(dt_samples_wide, c("k", "p", ".draw"))

  dt_samples_wide_adj <- adjust_parameters(dt_samples_wide)

  dt_times <- data.table(t = time_range)

  # Artificial time IDs so merge creates all time points for each sample
  dt_times[, t_id := 1, by = t]
  dt_samples_wide_adj[, t_id := 1]

  dt_out <- merge(
    dt_samples_wide_adj, dt_times, by = "t_id", allow.cartesian = TRUE)

  dt_out[, mu := simulate_trajectory(
    t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
           by = c("t", "p", "k", ".draw")]

  if (summarise == TRUE) {
    dt_out <- summarise_draws(
      dt_out, column_name = "mu", by = c("t", "p", "k"))
  }

  return(dt_out)
}

summarise_draws <- function(dt_in, column_name, by = by) {
  # Declare variables to suppress notes when compiling package
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  . <- NULL

  dt_out <- dt_in[, .(
    me = stats::quantile(get(column_name), 0.5),
    lo = stats::quantile(get(column_name), 0.025),
    hi = stats::quantile(get(column_name), 0.975)
  ),
                    by = by
  ]

  return(dt_out)
}
