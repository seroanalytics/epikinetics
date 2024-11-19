##' @title Biomarker Kinetics Model
##'
##' @description Create an instance of the biomarker kinetics model and
##' fit it to a dataset.
##' @export
##' @importFrom R6 R6Class
biokinetics <- R6::R6Class(
  "biokinetics",
  cloneable = FALSE,
  private = list(
    scale = NULL,
    priors = NULL,
    preds_sd = NULL,
    upper_censoring_limit = NULL,
    lower_censoring_limit = NULL,
    smallest_value = NULL,
    data = NULL,
    covariate_formula = NULL,
    fitted = NULL,
    stan_input_data = NULL,
    model = NULL,
    all_formula_vars = NULL,
    design_matrix = NULL,
    covariate_lookup_table = NULL,
    pid_lookup = NULL,
    titre_type_lookup = NULL,
    check_fitted = function() {
      if (is.null(private$fitted)) {
        stop("Model has not been fitted yet. Call 'fit' before calling this function.")
      }
    },
    get_upper_censoring_limit = function(upper_censoring_limit) {
      if (is.null(upper_censoring_limit)) {
        private$upper_censoring_limit <- max(private$data$value)
      } else {
        private$upper_censoring_limit <- upper_censoring_limit
      }
    },
    get_lower_censoring_limit = function(lower_censoring_limit) {
      if (is.null(lower_censoring_limit)) {
        private$lower_censoring_limit <- min(private$data$value)
      } else {
        private$lower_censoring_limit <- lower_censoring_limit
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
      var <- pid <- NULL
      dt_design_matrix <- private$data[, .SD, .SDcols = private$all_formula_vars, by = pid] |>
        unique()

      # Build the full design matrix using model.matrix
      mm <- private$model_matrix_with_dummy(data = dt_design_matrix)

      # Identify columns with no variance and remove them
      variance_per_column <- apply(mm, 2, var)
      relevant_columns <- which(variance_per_column != 0)
      mm_reduced <- mm[, relevant_columns, drop = FALSE]
      private$design_matrix <- mm_reduced
    },
    build_covariate_lookup_table = function() {
      private$covariate_lookup_table <- build_covariate_lookup_table(private$data,
                                                                     private$design_matrix,
                                                                     private$all_formula_vars)
    },
    build_pid_lookup = function() {
      private$pid_lookup <- build_pid_lookup(private$data)
    },
    build_titre_type_lookup = function() {
      private$titre_type_lookup <- build_titre_type_lookup(private$data)
    },
    recover_covariate_names = function(dt) {
      dt_out <- dt[, titre_type := names(private$titre_type_lookup)[k]][, `:=`(k = NULL)]
      if ("p" %in% colnames(dt)) {
        dt_out <- dt_out[private$covariate_lookup_table, on = "p", nomatch = NULL][, `:=`(p = NULL)]
      }
      data.table::setnames(dt_out, "t", "time_since_last_exp", skip_absent = TRUE)
      dt_out
    },
    summarise_pop_fit = function(time_range,
                                 summarise,
                                 n_draws) {

      has_covariates <- length(private$all_formula_vars) > 0

      # Declare variables to suppress notes when compiling package
      # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
      t0_pop <- tp_pop <- ts_pop <- m1_pop <- m2_pop <- m3_pop <- NULL
      k <- p <- .draw <- t_id <- mu <- NULL

      params <- c("t0_pop[k]", "tp_pop[k]", "ts_pop[k]",
                  "m1_pop[k]", "m2_pop[k]", "m3_pop[k]")
      if (has_covariates) {
        params <- c(params, "beta_t0[p]", "beta_tp[p]", "beta_ts[p]",
                    "beta_m1[p]", "beta_m2[p]", "beta_m3[p]")
      }

      params_proc <- rlang::parse_exprs(params)

      dt_samples_wide <- tidybayes::spread_draws(
        private$fitted, !!!params_proc) |>
        data.table()

      dt_samples_wide <- dt_samples_wide[.draw %in% 1:n_draws]
      dt_samples_wide[, `:=`(.chain = NULL, .iteration = NULL)]

      if (!has_covariates) {
        # there are no covariates, so add dummy column
        # that will be removed after processing
        dt_samples_wide$p <- 1
      }

      data.table::setcolorder(dt_samples_wide, c("k", "p", ".draw"))

      if (has_covariates) {
        logger::log_info("Adjusting by regression coefficients")
        dt_samples_wide <- private$adjust_parameters(dt_samples_wide)
      }

      # adding artificial ids so that we can do a big merge, adding times to
      # each set of parameter samples
      dt_times <- data.table(t = time_range)
      dt_times[, t_id := 1, by = t]
      dt_samples_wide[, t_id := 1]

      dt_out <- merge(
        dt_samples_wide, dt_times, by = "t_id", allow.cartesian = TRUE)

      dt_out[, mu := biokinetics_simulate_trajectory(
        t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
               by = c("t", "p", "k", ".draw")]

      dt_out[, t_id := NULL]

      if (summarise == TRUE) {
        logger::log_info("Summarising into quantiles")
        dt_out <- summarise_draws(
          dt_out, column_name = "mu", by = c("t", "p", "k"))
      }

      data.table::setcolorder(dt_out, c("t", "p", "k"))

      if (!has_covariates) {
        dt_out[, p := NULL]
      }
      dt_out
    },
    prepare_stan_data = function() {
      pid <- value <- censored_lo <- censored_hi <- titre_type <- obs_id <- time_since_last_exp <- NULL
      stan_data <- list(
        N = private$data[, .N],
        N_events = private$data[, data.table::uniqueN(pid)],
        id = private$data[, private$pid_lookup[pid]],
        value = private$data[, value],
        titre_type = private$data[, private$titre_type_lookup[titre_type]],
        preds_sd = private$preds_sd,
        K = private$data[, data.table::uniqueN(titre_type)],
        N_uncens = private$data[!censored_lo & !censored_hi, .N],
        N_lo = private$data[censored_lo == TRUE, .N],
        N_hi = private$data[censored_hi == TRUE, .N],
        uncens_idx = private$data[censored_lo == FALSE & censored_hi == FALSE, obs_id],
        cens_lo_idx = private$data[censored_lo == TRUE, obs_id],
        cens_hi_idx = private$data[censored_hi == TRUE, obs_id])

      stan_data$t <- private$data[, time_since_last_exp]
      stan_data$X <- private$design_matrix
      stan_data$P <- ncol(private$design_matrix)
      if (private$scale == "natural") {
        # do the same transformation as used on the data
        stan_data$upper_censoring_limit <- log2(private$upper_censoring_limit / private$smallest_value)
        stan_data$lower_censoring_limit <- log2(private$lower_censoring_limit / private$smallest_value)
      } else {
        stan_data$upper_censoring_limit <- private$upper_censoring_limit
        stan_data$lower_censoring_limit <- private$lower_censoring_limit
      }
      private$stan_input_data <- c(stan_data, private$priors)
    },
    adjust_parameters = function(dt) {
      params_to_adjust <- c(
        "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop")
      # Loop through the parameters you want to adjust
      for (param in params_to_adjust) {
        # Remove the '_pop' suffix to construct the beta variable name
        beta_var <- paste0("beta_", gsub("_pop$", "", param))
        dt[, (param) := get(param) + get(beta_var)]
      }

      return(dt)
    },
    extract_parameters = function(params, n_draws) {
      private$check_fitted()
      params_proc <- rlang::parse_exprs(params)

      dt_out <- tidybayes::spread_draws(private$fitted, !!!params_proc) |>
        data.table()

      dt_out[, `:=`(.chain = NULL, .iteration = NULL)]

      dt_out[.draw %in% 1:n_draws]
    }
  ),
  public = list(
    #' @description Initialise the kinetics model.
    #' @return An epikinetics::biokinetics object.
    #' @param data Optional data table of model inputs. One of data or file must be provided. See the data vignette
    #' for required columns: \code{vignette("data", package = "epikinetics")}.
    #' @param file_path Optional file path to model inputs in CSV format. One of data or file must be provided.
    #' @param priors Object of type \link[epikinetics]{biokinetics_priors}. Default biokinetics_priors().
    #' @param covariate_formula Formula specifying linear regression model. Note all variables in the formula
    #' will be treated as categorical variables. Default ~0.
    #' @param preds_sd Standard deviation of predictor coefficients. Default 0.25.
    #' @param scale One of "log" or "natural". Default "natural". Is provided data on a log or a natural scale? If on a natural scale it
    #' will be converted to a log scale for model fitting.
    #' @param upper_censoring_limit Optional value at which to upper censor data points. This is needed to construct a likelihood for upper censored
    #' values, so only needs to be provided if you have such values in the dataset. If not provided, no censoring will be done.
    #' @param lower_censoring_limit Optional value at which to lower censor data points. This is needed to construct a likelihood for lower censored
    #' values, so only needs to be provided if you have such values in the dataset. If not provided, no censoring will be done.
    #' @param strict_upper_limit Logical. Whether values greater than the upper censoring limit should be censored.
    #' If FALSE, only values exactly equal to the upper censoring limit will be censored. Default TRUE.
    #' @param strict_lower_limit Logical. Whether values smaller than the lower censoring limit should be censored.
    #' If FALSE, only values exactly equal to the lower censoring limit will be censored. Default TRUE.
    initialize = function(priors = biokinetics_priors(),
                          data = NULL,
                          file_path = NULL,
                          covariate_formula = ~0,
                          preds_sd = 0.25,
                          scale = "natural",
                          upper_censoring_limit = NULL,
                          lower_censoring_limit = NULL,
                          strict_upper_limit = TRUE,
                          strict_lower_limit = TRUE) {
      validate_priors(priors)
      private$priors <- priors
      validate_numeric(preds_sd)
      private$preds_sd <- preds_sd
      validate_formula(covariate_formula)
      private$covariate_formula <- covariate_formula
      private$all_formula_vars <- all.vars(covariate_formula)
      private$scale <- scale
      if (is.null(data) && is.null(file_path)) {
        stop("One of 'data' or 'file_path' must be provided")
      }
      if (!is.null(data) && !is.null(file_path)) {
        stop("Only one of 'data' or 'file_path' should be provided")
      }
      if (is.null(data)) {
        private$data <- data.table::fread(file_path)
      } else {
        if (!data.table::is.data.table(data)) {
          stop("'data' must be a data.table")
        }
        private$data <- data
      }
      validate_required_cols(private$data)
      validate_formula_vars(private$all_formula_vars, private$data)
      logger::log_info("Preparing data for stan")
      private$get_upper_censoring_limit(upper_censoring_limit)
      private$get_lower_censoring_limit(lower_censoring_limit)
      max_value <- max(private$data$value)
      min_value <- min(private$data$value)
      values_above <- max_value > private$upper_censoring_limit
      values_below <- min_value < private$lower_censoring_limit
      if (strict_upper_limit) {
        if (values_above) {
          warning(sprintf("Data contains values above the upper censoring limit %s and these will be censored. To turn off this behaviour set strict_upper_limit to FALSE.",
                          private$upper_censoring_limit))
        }
        private$data[, value := ifelse(value > private$upper_censoring_limit, private$upper_censoring_limit, value)]
      } else if (values_above) {
        warning(sprintf("Data contains values above the upper censoring limit %s. To treat these as censored set strict_upper_limit to TRUE.",
                        private$upper_censoring_limit))
      }
      if (strict_lower_limit) {
        if (values_below) {
          warning(sprintf("Data contains values below the lower censoring limit %s and these will be censored. To turn off this behaviour set strict_lower_limit to FALSE.",
                          private$lower_censoring_limit))
        }
        private$data[, value := ifelse(value < private$lower_censoring_limit, private$lower_censoring_limit, value)]
      } else if (values_below) {
        warning(sprintf("Data contains values below the lower censoring limit %s. To treat these as censored set strict_lower_limit to TRUE.",
                        private$lower_censoring_limit))
      }
      private$data[, `:=`(obs_id = seq_len(.N),
                          time_since_last_exp = as.integer(day - last_exp_day, units = "days"),
                          censored_lo = value == private$lower_censoring_limit,
                          censored_hi = value == private$upper_censoring_limit)]
      private$construct_design_matrix()
      private$build_covariate_lookup_table()
      private$build_pid_lookup()
      private$build_titre_type_lookup()
      if (scale == "natural") {
        private$smallest_value <- min(private$data$value)
        private$data <- convert_log2_scale(private$data,
                                           smallest_value = private$smallest_value,
                                           vars_to_transform = "value")
      }
      private$prepare_stan_data()
      logger::log_info("Retrieving compiled model")
      private$model <- instantiate::stan_package_model(
        name = "antibody_kinetics_main",
        package = "epikinetics"
      )
    },
    #' @description Plot the kinetics trajectory predicted by the model priors.
    #' Note that this is on a log scale, regardless of whether the data was provided
    #' on a log or a natural scale.
    #' @return A ggplot2 object.
    #' @param tmax Integer. The number of time points in each simulated trajectory. Default 150.
    #' @param n_draws Integer. The number of trajectories to simulate. Default 2000.
    plot_prior_predictive = function(tmax = 150,
                                     n_draws = 2000) {
      plot(private$priors,
           tmax = tmax,
           n_draws = n_draws,
           data = private$data,
           upper_censoring_limit = private$stan_input_data$upper_censoring_limit,
           lower_censoring_limit = private$stan_input_data$lower_censoring_limit)
    },
    #' @description Plot model input data with a smoothing function. Note that
    #' this plot is of the data as provided to the Stan model so is on a log scale,
    #' regardless of whether data was provided on a log or a natural scale.
    #' @param tmax Integer. Maximum time since last exposure to include. Default 150.
    #' @return A ggplot2 object.
    plot_model_inputs = function(tmax = 150) {
      plot_sero_data(private$data,
                     tmax = tmax,
                     covariates = private$all_formula_vars,
                     upper_censoring_limit = private$stan_input_data$upper_censoring_limit,
                     lower_censoring_limit = private$stan_input_data$lower_censoring_limit)
    },
        #' @description View the data that is passed to the stan model, for debugging purposes.
        #' @return A list of arguments that will be passed to the stan model.
    get_stan_data = function() {
      private$stan_input_data
    },
        #' @description View the mapping of human readable covariate names to the model variable p.
        #' @return A data.table mapping the model variable p to human readable covariates.
    get_covariate_lookup_table = function() {
      private$covariate_lookup_table
    },
        #' @description Fit the model and return CmdStanMCMC fitted model object.
        #' @return A CmdStanMCMC fitted model object: <https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html>
        #' @param ... Named arguments to the `sample()` method of CmdStan model.
        #'   objects: <https://mc-stan.org/cmdstanr/reference/model-method-sample.html>
    fit = function(...) {
      logger::log_info("Fitting model")
      private$fitted <- private$model$sample(private$stan_input_data, ...)
      private$fitted
    },
        #' @description Extract fitted population parameters
        #' @return A data.table
        #' @param n_draws Integer. Default 2000.
        #' @param human_readable_covariates Logical. Default TRUE.
    extract_population_parameters = function(n_draws = 2000,
                                             human_readable_covariates = TRUE) {
      private$check_fitted()
      has_covariates <- length(private$all_formula_vars) > 0

      params <- c("t0_pop[k]", "tp_pop[k]", "ts_pop[k]", "m1_pop[k]", "m2_pop[k]", "m3_pop[k]")

      if (has_covariates) {
        params <- c(params, "beta_t0[p]", "beta_tp[p]", "beta_ts[p]", "beta_m1[p]", "beta_m2[p]", "beta_m3[p]")
      }

      logger::log_info("Extracting parameters")
      dt_out <- private$extract_parameters(params, n_draws)

      if (has_covariates) {
        data.table::setcolorder(dt_out, c("p", "k", ".draw"))
      } else {
        data.table::setcolorder(dt_out, c("k", ".draw"))
      }

      data.table::setnames(dt_out, ".draw", "draw")

      if (length(private$all_formula_vars) > 0) {
        logger::log_info("Adjusting by covariates")
        dt_out <- private$adjust_parameters(dt_out)
      }
      if (human_readable_covariates) {
        logger::log_info("Recovering covariate names")
        dt_out <- private$recover_covariate_names(dt_out)
      }
      dt_out
    },
    #' @description Extract fitted individual parameters
    #' @return A data.table
    #' @param n_draws Integer. Default 2000.
    #' @param include_variation_params Logical
    #' @param human_readable_covariates Logical. Default TRUE.
    extract_individual_parameters = function(n_draws = 2000,
                                             include_variation_params = TRUE,
                                             human_readable_covariates = TRUE) {
      private$check_fitted()
      params <- c("t0_ind[n, k]", "tp_ind[n, k]", "ts_ind[n, k]",
                  "m1_ind[n, k]", "m2_ind[n, k]", "m3_ind[n, k]")

      if (include_variation_params == TRUE) {
        ind_var_params <- c(
          "z_t0[n]", "z_tp[n]", "z_ts[n]", "z_m1[n]", "z_m2[n]", "z_m3[n]")
        params <- c(params, ind_var_params)
      }

      logger::log_info("Extracting parameters")
      dt_out <- private$extract_parameters(params, n_draws)

      data.table::setnames(dt_out, ".draw", "draw")

      dt_out[, pid := names(private$pid_lookup)[n]]
      if (is.numeric(private$data$pid)) {
        dt_out[, pid := as.numeric(pid)]
      }
      dt_out$n <- NULL

      data.table::setcolorder(dt_out, c("pid", "k", "draw"))

      if (human_readable_covariates) {
        logger::log_info("Recovering covariate names")
        dt_out <- private$recover_covariate_names(dt_out)
      }
      dt_out
    },
    #' @description Process the model results into a data table of titre values over time.
    #' @return A data.table containing titre values at time points. If summarise = TRUE, columns are time_since_last_exp,
    #' me, lo, hi, titre_type, and a column for each covariate in the hierarchical model. If summarise = FALSE, columns are
    #' time_since_last_exp, .draw, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop, beta_t0, beta_tp, beta_ts, beta_m1, beta_m2, beta_m3, mu
    #' titre_type and a column for each covariate in the hierarchical model. See the data vignette for details:
    #' \code{vignette("data", package = "epikinetics")}
    #' @param t_max Integer. Maximum number of time points to include.
    #' @param summarise Boolean. Default TRUE. If TRUE, summarises over draws from posterior parameter distributions to
    #' return 0.025, 0.5 and 0.975 quantiles, labelled lo, me and hi, respectively. If FALSE returns values for individual
    #' draws from posterior parameter distributions.
    #' @param n_draws Integer. Maximum number of samples to include. Default 2000.
    simulate_population_trajectories = function(
      t_max = 150,
      summarise = TRUE,
      n_draws = 2000) {

      private$check_fitted()
      validate_numeric(t_max)
      validate_logical(summarise)
      validate_numeric(n_draws)

      logger::log_info("Summarising fits")
      dt_sum <- private$summarise_pop_fit(
        time_range = seq(0, t_max),
        summarise = summarise,
        n_draws = n_draws)

      dt_out <- private$recover_covariate_names(dt_sum)

      dt_out <- dt_out[
        , lapply(.SD, function(x) if (is.factor(x)) forcats::fct_drop(x) else x)]

      if (private$scale == "natural") {
        if (summarise) {
          dt_out <- convert_log2_scale_inverse(
            dt_out,
            vars_to_transform = c("me", "lo", "hi"),
            smallest_value = private$smallest_value)
        } else {
          dt_out <- convert_log2_scale_inverse(
            dt_out,
            vars_to_transform = "mu",
            smallest_value = private$smallest_value)
        }
      }

      class(dt_out) <- append("biokinetics_population_trajectories", class(dt_out))
      attr(dt_out, "summarised") <- summarise
      attr(dt_out, "scale") <- private$scale
      attr(dt_out, "covariates") <- private$all_formula_vars
      attr(dt_out, "upper_censoring_limit") <- private$upper_censoring_limit
      attr(dt_out, "lower_censoring_limit") <- private$lower_censoring_limit

      dt_out
    },
    #' @description Process the stan model results into a data.table.
    #' @return A data.table of peak and set titre values. Columns are tire_type, mu_p, mu_s, rel_drop_me, mu_p_me,
    #' mu_s_me, and a column for each covariate. See the data vignette for details:
    #' \code{vignette("data", package = "epikinetics")}
    #' @param n_draws Integer. Maximum number of samples to include. Default 2000.
    population_stationary_points = function(n_draws = 2000) {
      private$check_fitted()
      validate_numeric(n_draws)

      dt_peak_switch <- self$extract_population_parameters(n_draws,
                                                           human_readable_covariates = FALSE)

      logger::log_info("Calculating peak and switch titre values")

      by <- c("k", "draw")
      if ("p" %in% colnames(dt_peak_switch)) {
        by <- c("p", by)
      }

      dt_peak_switch[, `:=`(
        mu_0 = biokinetics_simulate_trajectory(
          0, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
        mu_p = biokinetics_simulate_trajectory(
          tp_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
        mu_s = biokinetics_simulate_trajectory(
          ts_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop)),
                       by = by]

      logger::log_info("Recovering covariate names")
      dt_peak_switch <- private$recover_covariate_names(dt_peak_switch)

      if (private$scale == "natural") {
        dt_peak_switch <- convert_log2_scale_inverse(
          dt_peak_switch,
          vars_to_transform = c("mu_0", "mu_p", "mu_s"),
          smallest_value = private$smallest_value)
      }

      logger::log_info("Calculating medians")
      dt_peak_switch[
        , rel_drop := mu_s / mu_p,
          by = c(private$all_formula_vars, "titre_type")][
        , .(
        mu_p,
        mu_s,
        rel_drop_me = quantile(rel_drop, 0.5),
        mu_p_me = quantile(mu_p, 0.5),
        mu_s_me = quantile(mu_s, 0.5)),
          by = c(private$all_formula_vars, "titre_type")]
    },
    #' @description Simulate individual trajectories from the model. This is
    #' computationally expensive and may take a while to run if n_draws is large.
    #' @return A data.table. If summarise = TRUE columns are calendar_date, titre_type, me, lo, hi, time_shift.
    #' If summarise = FALSE, columns are pid, draw, time_since_last_exp, mu, titre_type, exposure_day, calendar_day, time_shift
    #' and a column for each covariate in the regression model. See the data vignette for details:
    #' \code{vignette("data", package = "epikinetics")}.
    #' @param summarise Boolean. If TRUE, average the individual trajectories to get lo, me and
    #' hi values for the population, disaggregated by titre type. If FALSE return the indidivudal trajectories.
    #' Default TRUE.
    #' @param n_draws Integer. Maximum number of samples to draw. Default 2000.
    #' @param time_shift Integer. Number of days to adjust the exposure day by. Default 0.
    simulate_individual_trajectories = function(
      summarise = TRUE,
      n_draws = 2000,
      time_shift = 0) {
      private$check_fitted()
      validate_logical(summarise)
      validate_numeric(n_draws)
      validate_numeric(time_shift)

      # Extracting parameters from fit
      dt_params_ind <- self$extract_individual_parameters(n_draws,
                                                          human_readable_covariates = FALSE,
                                                          include_variation_params = FALSE)[!is.nan(t0_ind)]

      # Calculating the maximum time each individual has data for after the
      # exposure of interest
      dt_max_dates <- private$data[
        , .(t_max = max(time_since_last_exp)), by = "pid"]

      # A very small number of individuals have bleeds on the same day or a few days
      # after their recorded exposure dates, resulting in very short trajectories.
      # Adding a 50 day buffer to any individuals with less than or equal to 50 days
      # of observations after their focal exposure
      dt_max_dates <- dt_max_dates[t_max <= 50, t_max := 50, by = "pid"]

      # Merging the parameter draws with the maximum time data.table
      dt_params_ind <- merge(dt_params_ind, dt_max_dates, by = "pid")

      # convert original pid to numeric pid
      dt_params_ind[, pid := private$pid_lookup[pid]]

      # Running the C++ code to simulate trajectories for each parameter sample
      # for each individual
      logger::log_info("Simulating individual trajectories")
      dt_params_ind_traj <- biokinetics_simulate_trajectories(dt_params_ind)

      if (private$scale == "natural") {
        dt_params_ind_traj <- data.table::setDT(convert_log2_scale_inverse_cpp(
          dt_params_ind_traj,
          vars_to_transform = "mu",
          smallest_value = private$smallest_value))
      }

      # convert numeric pid to original pid
      dt_params_ind_traj[, pid := names(private$pid_lookup)[pid]]
      if (is.numeric(private$data$pid)) {
        dt_params_ind_traj[, pid := as.numeric(pid)]
      }

      logger::log_info("Recovering covariate names")
      dt_params_ind_traj <- private$recover_covariate_names(dt_params_ind_traj)

      logger::log_info(paste("Calculating exposure days. Adjusting exposures by", time_shift, "days"))
      dt_lookup <- private$data[, .(
        exposure_day = min(last_exp_day) - time_shift),
                                  by = c(private$all_formula_vars, "pid")]

      dt_out <- merge(dt_params_ind_traj, dt_lookup, by = "pid")

      dt_out[
        , calendar_day := exposure_day + time_since_last_exp,
          by = c(private$all_formula_vars, "pid", "titre_type")]

      if (summarise) {
        logger::log_info("Resampling")
        dt_out <- dt_out[
          !is.nan(mu), .(pop_mu_sum = mean(mosaic::resample(mu))),
          by = c("calendar_day", "draw", "titre_type")]

        logger::log_info("Summarising into population quantiles")
        dt_out <- summarise_draws(
          dt_out,
          column_name = "pop_mu_sum",
          by = c("calendar_day", "titre_type"))
      }

      dt_out <- dt_out[, time_shift := time_shift]
      class(dt_out) <- append("biokinetics_individual_trajectories", class(dt_out))
      attr(dt_out, "summarised") <- summarise
      dt_out
    }
  )
)
