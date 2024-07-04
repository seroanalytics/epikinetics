##' @title SARS-CoV-2 Antibody Kinetics Model
##'
##' @description Create an instance of the SARS-CoV-2 antibody model and
##' fit it to a dataset using Gaussian priors and a given hierarchical model structure.
##' @export
##' @importFrom R6 R6Class
sam <- R6::R6Class(
  "sam",
  cloneable = FALSE,
  private = list(
    priors = NULL,
    preds_sd = NULL,
    data = NULL,
    covariate_formula = NULL,
    time_type = NULL,
    fitted = NULL,
    stan_input_data = NULL,
    model = NULL,
    all_formula_vars = NULL,
    design_matrix = NULL,
    covariate_lookup_table = NULL,
    check_fitted = function() {
      if (is.null(private$fitted)) {
        stop("Model has not been fitted yet. Call 'run' before calling this function.")
      }
    },
    model_matrix_with_dummy = function(data) {
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
      mm <- stats::model.matrix(private$covariate_formula, data)

      # If dummy row was added, remove the corresponding row from the model matrix
      if (any(single_level_factors)) {
        mm <- mm[-nrow(mm), , drop = FALSE]
      }

      mm
    },
    construct_design_matrix = function() {
      var <- stan_id <- NULL
      dt_design_matrix <- private$data[, .SD, .SDcols = private$all_formula_vars, by = stan_id] |>
        unique()

      # Build the full design matrix using model.matrix
      mm <- private$model_matrix_with_dummy(data = dt_design_matrix)

      # Identify columns with no variance and remove them
      variance_per_column <- apply(mm, 2, var)
      relevant_columns <- which(variance_per_column != 0)
      mm_reduced <- mm[, relevant_columns]
      private$design_matrix <- mm_reduced
      mm_reduced
    },
    build_covariate_lookup_table = function() {
      # Extract column names
      col_names <- colnames(private$design_matrix)

      # Split column names based on the ':' delimiter
      split_data <- stringr::str_split(col_names, ":", simplify = TRUE)

      # Convert the matrix to a data.table
      dt <- data.table::as.data.table(split_data)

      # Set the new column names
      data.table::setnames(dt, private$all_formula_vars)

      for (col_name in names(dt)) {
        # Find the matching formula variable for current column
        matching_formula_var <- private$all_formula_vars[which(startsWith(col_name, private$all_formula_vars))]
        if (length(matching_formula_var) > 0) {
          pattern_to_remove <- paste0("^", matching_formula_var)
          dt[, (col_name) := stringr::str_remove_all(get(col_name), pattern_to_remove)]
        }
      }

      # Declare variables to suppress notes when compiling package
      # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
      p <- NULL

      # .I is a special symbol in data.table for row number
      dt[, p := .I]

      # Reorder columns to have 'i' first
      data.table::setcolorder(dt, "p")
      private$covariate_lookup_table <- dt
      dt
    },
    recover_covariate_names = function(dt) {

      # Declare variables to suppress notes when compiling package
      # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
      titre_type <- NULL

      dt_titre_lookup <- data.table(
        k = 1:private$data[, length(unique(titre_type))],
        titre_type = private$data[, unique(titre_type)])

      dt[
        private$covariate_lookup_table, on = "p"][
        dt_titre_lookup, on = "k"]
    },
    summarise_pop_fit = function(
      time_range = seq(0, 200, 1),
      summarise = TRUE,
      n_draws = 2500) {

      # Declare variables to suppress notes when compiling package
      # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
      t0_pop <- tp_pop <- ts_pop <- m1_pop <- m2_pop <- m3_pop <- NULL
      beta_t0 <- beta_tp <- beta_ts <- beta_m1 <- beta_m2 <- beta_m3 <- NULL
      k <- p <- .draw <- t_id <- mu <- NULL

      dt_samples_wide <- tidybayes::spread_draws(
        private$fitted,
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

      dt_out[, mu := private$simulate_trajectory(
        t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
               by = c("t", "p", "k", ".draw")]

      if (summarise == TRUE) {
        dt_out <- summarise_draws(
          dt_out, column_name = "mu", by = c("t", "p", "k"))
      }

      dt_out
    },
    simulate_trajectory = function(t, t0, tp, ts, m1, m2, m3) {
      mu <- t0
      if (t < tp) {
        mu <- mu + m1 * t
      } else if (t <= ts) {
        mu <- mu + m1 * tp + m2 * (t - tp)
      } else if (t > ts) {
        mu <- mu + m1 * tp + m2 * (ts - tp) + m3 * (t - ts)
      }
      max(mu, 0)
    },
    prepare_stan_data = function() {
      stan_id <- titre <- censored <- titre_type_num <- titre_type <- obs_id <- t_since_last_exp <- t_since_min_date <- NULL
      stan_data <- list(
        N = private$data[, .N],
        N_events = private$data[, data.table::uniqueN(stan_id)],
        id = private$data[, stan_id],
        titre = private$data[, titre],
        censored = private$data[, censored],
        titre_type = private$data[, titre_type_num],
        preds_sd = private$preds_sd,
        K = private$data[, data.table::uniqueN(titre_type)],
        N_uncens = private$data[censored == 0, .N],
        N_lo = private$data[censored == -2, .N],
        N_me = private$data[censored == -1, .N],
        N_hi = private$data[censored == 1, .N],
        uncens_idx = private$data[censored == 0, obs_id],
        cens_lo_idx = private$data[censored == -2, obs_id],
        cens_me_idx = private$data[censored == -1, obs_id],
        cens_hi_idx = private$data[censored == 1, obs_id])

      if (private$time_type == "relative") {
        stan_data$t <- private$data[, t_since_last_exp]
      } else {
        stan_data$t <- private$data[, t_since_min_date]
      }

      X <- private$construct_design_matrix()
      stan_data$X <- X
      stan_data$P <- ncol(X)

      c(stan_data, private$priors)
    },

    extract_parameters_pop = function(n_draws = 2500) {

      params <- c("t0_pop[k]", "tp_pop[k]", "ts_pop[k]", "m1_pop[k]", "m2_pop[k]",
                  "m3_pop[k]", "beta_t0[p]", "beta_tp[p]", "beta_ts[p]", "beta_m1[p]",
                  "beta_m2[p]", "beta_m3[p]")

      params_proc <- rlang::parse_exprs(params)

      dt_proc <- tidybayes::spread_draws(private$fitted, !!!params_proc) |>
        data.table::data.table()

      dt_proc[, `:=`(.chain = NULL, .iteration = NULL)]

      data.table::setcolorder(dt_proc, c("k", "p", ".draw"))

      if (adjust == TRUE) {
        dt_out <- adjust_parameters(dt_proc)
      } else {
        dt_out <- dt_proc
      }

      dt_out[.draw %in% n_draws]
    }
  ),
  public = list(
    #' @description Initialise the kinetics model and return fitted model.
    #' @return An epikinetics::sam object.
    #' @param data Optional data table of model inputs. One of data or file must be provided.
    #' @param file_path Optional file path to model inputs in CSV format. One of data or file must be provided.
    #' @param priors Object of type 'sam_priors'. Default sam_priors().
    #' @param covariate_formula Formula specifying hierarchical structure of model. Default ~0 + infection_history.
    #' @param preds_sd Standard deviation of predictor coefficients. Default 0.25.
    #' @param time_type One of 'relative' or 'absolute'. Default 'relative'.
    initialize = function(priors = sam_priors(),
                          data = NULL,
                          file_path = NULL,
                          covariate_formula = ~0 + infection_history,
                          preds_sd = 0.25,
                          time_type = "relative") {
      if (!inherits(priors, "sam_priors")) {
        stop("'priors' must be of type 'sam_priors'")
      }
      private$priors <- priors
      if (!is.numeric(preds_sd)) {
        stop("'preds_sd' must be a number")
      }
      private$preds_sd <- preds_sd
      validate_time_type(time_type)
      private$time_type <- time_type
      if (!(class(covariate_formula) == "formula")) {
        stop("'covariate_formula' must be a formula")
      }
      private$covariate_formula <- covariate_formula
      private$all_formula_vars <- all.vars(covariate_formula)
      if (is.null(data) && is.null(file_path)) {
        stop("One of 'data' or 'file_path' must be provided")
      }
      if (!is.null(data) && !is.null(file_path)) {
        stop("Only one of 'data' or 'file_path' should be provided")
      }
      if (is.null(data)) {
        private$data <- data.table::fread(file_path)
      } else {
        if (!is.data.frame(data)) {
          stop("'data' must be a data frame")
        }
        private$data <- data
      }
      logger::log_info("Preparing stan data")
      private$stan_input_data <- private$prepare_stan_data()
      private$build_covariate_lookup_table()
      logger::log_info("Retrieving compiled model")
      private$model <- instantiate::stan_package_model(
        name = "antibody_kinetics_main",
        package = "epikinetics"
      )
    },
    #' @description Fit the model and return CmdStanMCMC fitted model object.
    #' @return A CmdStanMCMC fitted model object.
    #' @param ... Named arguments to the `sample()` method of CmdStan model.
    #'   objects: <https://mc-stan.org/cmdstanr/reference/model-method-sample.html>
    fit = function(...) {
      private$fitted <- private$model$sample(private$stan_input_data, ...)
      private$fitted
    },
    #' @description Process the model results into a data table of titre values over time.
    #' @return A Table containing titre values at time points.
    #' @param time_type One of 'relative' or 'absolute'. Default 'relative'.
    #' @param t_max Numeric. Maximum number of time points to include.
    #' @param summarise Boolean. Default TRUE. If TRUE returns values for 0.025, 0.5 and 0.975 quantiles, if FALSE returns
    #' individual values.
    #' @param scale One of 'natural' or 'log'. Default 'natural'.
    #' @param n_draws Integer. Number of samples to draw. Default 2500.
    population_trajectories = function(
      time_type = "relative",
      t_max = 150,
      summarise = TRUE,
      scale = "natural",
      n_draws = 2500) {

      private$check_fitted()
      validate_time_type(time_type)
      validate_scale(scale)

      dt_sum <- private$summarise_pop_fit(
        time_range = seq(0, t_max),
        summarise = summarise,
        n_draws = n_draws)

      dt_out <- private$recover_covariate_names(dt_sum)

      if (time_type == "absolute") {
        dt_out[, date := dt[, unique(min(date))] + t,
                 by = c(private$all_formula_vars, "titre_type")]
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

      dt_out
    },
    #' @description Process the stan model results into a data table.
    #' @return A Table.
    #' @param n_draws Integer. Number of samples to draw. Default 2500.
    population_stationary_points = function(
      n_draws = 2500) {

      private$check_fitted()
      # Extracting population-level parameters
      dt_peak_switch <- private$extract_parameters_pop(n_draws = 2500)

      # Calculating the peak and switch titre values stratified by covariates
      # and titre types
      dt_peak_switch[, `:=`(
        mu_0 = private$simulate_trajectory(
          0, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
        mu_p = private$simulate_trajectory(
          tp_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
        mu_s = private$simulate_trajectory(
          ts_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop)),
                       by = c("p", "k", ".draw")]

      # Convert back to natural units
      convert_log_scale_inverse(
        dt_peak_switch, vars_to_transform = c("mu_0", "mu_p", "mu_s"))

      # Recover which covariates were used in the inference
      private$recover_covariate_names(dt_peak_switch)
    }
  )
)
