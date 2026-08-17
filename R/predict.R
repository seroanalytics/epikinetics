kinetics_mean <- function(time, baseline, time_to_peak, waning_change_time,
                          boost_rate, early_waning_rate, late_waning_rate) {
  before_peak <- time <= time_to_peak
  before_change <- time <= waning_change_time
  out <- baseline + boost_rate * time
  out[!before_peak & before_change] <- baseline[!before_peak & before_change] +
    boost_rate[!before_peak & before_change] *
      time_to_peak[!before_peak & before_change] -
    early_waning_rate[!before_peak & before_change] *
      (time[!before_peak & before_change] -
         time_to_peak[!before_peak & before_change])
  out[!before_change] <- baseline[!before_change] +
    boost_rate[!before_change] * time_to_peak[!before_change] -
    early_waning_rate[!before_change] *
      (waning_change_time[!before_change] - time_to_peak[!before_change]) -
    late_waning_rate[!before_change] *
      (time[!before_change] - waning_change_time[!before_change])
  out
}

as_epikinetics_prepared <- function(x) {
  prepared <- if (inherits(x, "epikinetics_fit")) x$prepared else x
  if (!inherits(prepared, "epikinetics_data")) {
    stop("'x' must be an epikinetics_data or epikinetics_fit object.",
         call. = FALSE)
  }
  prepared
}

continuous_prediction_value <- function(value, rule) {
  if (rule == "median") stats::median(value) else mean(value)
}

#' Construct default covariate profiles for prediction
#'
#' Builds inspectable participant-level profiles from the formula and model
#' frame stored by [prepare_epikinetics_data()]. By default, categorical
#' profiles are the combinations observed among fitted participants; this
#' avoids silently extrapolating to unsupported combinations. Continuous
#' covariates are held at their participant-level median. Use
#' `categorical = "cartesian"` to request every combination of fitted factor
#' levels, or pass an explicit `newdata` data frame to [predict()].
#'
#' These are conditional profiles, not averages over the fitted covariate
#' distribution. Interactions and transformed terms are subsequently evaluated
#' with the original terms object and contrasts when [predict()] constructs the
#' numeric design matrix.
#'
#' @param x An `epikinetics_data` or `epikinetics_fit` object.
#' @param categorical Use combinations `"observed"` in the participant data or
#'   the full `"cartesian"` product of fitted categorical levels.
#' @param continuous Hold continuous covariates at their participant-level
#'   `"median"` (default) or `"mean"`.
#'
#' @return An ordinary data frame with one row per prediction profile. The
#'   `.profile` column is a stable row identifier; remaining columns are the
#'   original variables used by the model formula.
#' @export
#' @examples
#' dat <- utils::read.csv(
#'   system.file("extdata", "delta.csv", package = "epikinetics")
#' )
#' prepared <- prepare_epikinetics_data(
#'   dat,
#'   formula = ~ infection_history,
#'   lower_limit = 5,
#'   upper_limit = 2560
#' )
#' prediction_grid(prepared)
prediction_grid <- function(
    x,
    categorical = c("observed", "cartesian"),
    continuous = c("median", "mean")) {
  prepared <- as_epikinetics_prepared(x)
  categorical <- match.arg(categorical)
  continuous <- match.arg(continuous)
  variables <- all.vars(prepared$specification$formula)
  categorical_variables <- prepared$specification$categorical_variables
  continuous_variables <- prepared$specification$continuous_variables

  if (!length(variables)) {
    out <- data.frame(.profile = 1L)
  } else {
    participant_data <- prepared$participant_data
    if (!length(categorical_variables)) {
      profiles <- data.frame(row.names = 1L)
    } else if (categorical == "observed") {
      profiles <- unique(
        participant_data[categorical_variables],
        incomparables = FALSE
      )
      rownames(profiles) <- NULL
    } else {
      values <- lapply(categorical_variables, function(variable) {
        value <- participant_data[[variable]]
        if (is.factor(value)) levels(value) else unique(value)
      })
      names(values) <- categorical_variables
      profiles <- do.call(
        expand.grid,
        c(values, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
      )
      for (variable in intersect(categorical_variables,
                                 names(prepared$specification$xlevels))) {
        original <- participant_data[[variable]]
        profiles[[variable]] <- factor(
          profiles[[variable]],
          levels = prepared$specification$xlevels[[variable]],
          ordered = is.ordered(original)
        )
      }
    }

    for (variable in continuous_variables) {
      value <- if (continuous == "median" &&
                   variable %in% names(
                     prepared$specification$prediction_defaults
                   )) {
        prepared$specification$prediction_defaults[[variable]]
      } else {
        continuous_prediction_value(participant_data[[variable]], continuous)
      }
      profiles[[variable]] <- rep(
        value,
        nrow(profiles)
      )
    }
    profiles <- profiles[variables]
    out <- data.frame(.profile = seq_len(nrow(profiles)))
    out[variables] <- profiles
  }
  rownames(out) <- NULL
  attr(out, "categorical_rule") <- categorical
  attr(out, "continuous_rule") <- continuous
  attr(out, "semantics") <- "conditional population profiles"
  out
}

validate_prediction_newdata <- function(prepared, newdata) {
  covariate_variables <- all.vars(prepared$specification$formula)
  if (!is.data.frame(newdata) || !nrow(newdata)) {
    stop("'newdata' must be a non-empty data.frame.", call. = FALSE)
  }
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  missing <- setdiff(covariate_variables, names(newdata))
  if (length(missing)) {
    stop("'newdata' is missing formula variable(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (anyNA(newdata[covariate_variables])) {
    stop("Missing covariate values are not supported in 'newdata'.",
         call. = FALSE)
  }

  for (variable in intersect(
    covariate_variables,
    names(prepared$specification$xlevels)
  )) {
    fitted_levels <- prepared$specification$xlevels[[variable]]
    supplied <- unique(as.character(newdata[[variable]]))
    unknown <- setdiff(supplied, fitted_levels)
    if (length(unknown)) {
      stop(
        "Unknown factor level(s) in '", variable, "': ",
        paste(unknown, collapse = ", "), ". Fitted levels are: ",
        paste(fitted_levels, collapse = ", "), ".",
        call. = FALSE
      )
    }
    newdata[[variable]] <- factor(
      newdata[[variable]],
      levels = fitted_levels,
      ordered = is.ordered(prepared$participant_data[[variable]])
    )
  }
  newdata
}

build_prediction_design <- function(prepared, newdata = NULL) {
  covariate_variables <- all.vars(prepared$specification$formula)
  grid_is_default <- is.null(newdata)
  if (grid_is_default) {
    newdata <- prediction_grid(prepared)
  }
  categorical_rule <- if (grid_is_default) {
    attr(newdata, "categorical_rule")
  } else {
    "explicit newdata"
  }
  continuous_rule <- if (grid_is_default) {
    attr(newdata, "continuous_rule")
  } else {
    "explicit newdata"
  }
  newdata <- validate_prediction_newdata(prepared, newdata)

  model_frame <- tryCatch(
    stats::model.frame(
      prepared$specification$terms,
      data = newdata,
      xlev = prepared$specification$xlevels,
      na.action = stats::na.fail
    ),
    error = function(error) {
      stop("Could not construct the prediction design matrix: ",
           conditionMessage(error), call. = FALSE)
    }
  )
  matrix <- stats::model.matrix(
    prepared$specification$terms,
    model_frame,
    contrasts.arg = prepared$specification$contrasts
  )
  assignment <- attr(matrix, "assign")
  matrix <- matrix[, assignment != 0L, drop = FALSE]

  expected <- prepared$mappings$covariates
  missing_columns <- setdiff(expected, colnames(matrix))
  if (length(missing_columns)) {
    zeros <- matrix(
      0,
      nrow = nrow(matrix),
      ncol = length(missing_columns),
      dimnames = list(NULL, missing_columns)
    )
    matrix <- cbind(matrix, zeros)
  }
  unexpected <- setdiff(colnames(matrix), expected)
  if (length(unexpected)) {
    stop("'newdata' created unexpected design columns: ",
         paste(unexpected, collapse = ", "), ".", call. = FALSE)
  }
  matrix <- matrix[, expected, drop = FALSE]
  storage.mode(matrix) <- "double"

  profiles <- data.frame(.profile = seq_len(nrow(newdata)))
  if (length(covariate_variables)) {
    profiles[covariate_variables] <- newdata[covariate_variables]
  }
  list(
    matrix = matrix,
    profiles = profiles,
    default = grid_is_default,
    categorical_rule = categorical_rule,
    continuous_rule = continuous_rule
  )
}

with_epikinetics_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("'seed' must be NULL or one finite number.", call. = FALSE)
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(as.integer(seed))
  force(code)
}

profile_parameter_draws <- function(x, draw_ids, newdata, new_participant,
                                    seed = NULL, matrices = NULL,
                                    biomarkers = NULL) {
  design <- build_prediction_design(x$prepared, newdata)
  matrices <- if (is.null(matrices)) {
    extract_kinetic_draw_matrices(
      x,
      draw_ids,
      components = c("population", "participant_sd", "beta")
    )
  } else {
    matrices
  }
  population <- matrices$population
  beta <- matrices$beta
  participant_sd <- matrices$participant_sd
  biomarkers <- select_biomarkers(x$prepared, biomarkers)
  biomarker_indices <- match(biomarkers, x$prepared$mappings$biomarkers)

  make_draws <- function() {
    out <- vector("list", nrow(design$matrix) * length(biomarkers))
    position <- 0L
    for (profile_index in seq_len(nrow(design$matrix))) {
      covariate_effect <- lapply(.epikinetics_parameters, function(parameter) {
        if (!ncol(design$matrix)) {
          rep(0, length(draw_ids))
        } else {
          as.numeric(beta[[parameter]] %*% design$matrix[profile_index, ])
        }
      }) |>
        stats::setNames(.epikinetics_parameters)
      random_effect <- lapply(.epikinetics_parameters, function(parameter) {
        active <- x$prepared$mappings$participant_parameters
        if (is.null(active)) active <- .epikinetics_parameters
        if (new_participant && parameter %in% active) {
          stats::rnorm(length(draw_ids))
        } else {
          rep(0, length(draw_ids))
        }
      }) |>
        stats::setNames(.epikinetics_parameters)

      for (j in seq_along(biomarkers)) {
        k <- biomarker_indices[j]
        position <- position + 1L
        baseline <- population$baseline[, k] + covariate_effect$baseline +
          participant_sd$baseline[, k] * random_effect$baseline
        positive <- lapply(.epikinetics_positive_parameters, function(parameter) {
          population[[parameter]][, k] * exp(
            covariate_effect[[parameter]] +
              participant_sd[[parameter]][, k] * random_effect[[parameter]]
          )
        }) |>
          stats::setNames(.epikinetics_positive_parameters)
        out[[position]] <- data.frame(
          .draw = draw_ids,
          .profile = profile_index,
          biomarker = biomarkers[j],
          baseline = baseline,
          time_to_peak = positive$time_to_peak,
          waning_duration = positive$waning_duration,
          boost_rate = positive$boost_rate,
          early_waning_rate = positive$early_waning_rate,
          late_waning_rate = positive$late_waning_rate,
          stringsAsFactors = FALSE
        )
      }
    }
    out <- do.call(rbind, out)
    rownames(out) <- NULL
    out$biomarker <- as_ordered_biomarker(out$biomarker, x$prepared)
    profile_rows <- match(out$.profile, design$profiles$.profile)
    profile_columns <- setdiff(names(design$profiles), ".profile")
    if (length(profile_columns)) {
      out[profile_columns] <- design$profiles[
        profile_rows,
        profile_columns,
        drop = FALSE
      ]
    }
    attr(out, "prediction_grid") <- design$profiles
    attr(out, "grid_is_default") <- design$default
    attr(out, "categorical_rule") <- design$categorical_rule
    attr(out, "continuous_rule") <- design$continuous_rule
    out
  }
  with_epikinetics_seed(seed, make_draws())
}

summarise_prediction_draws <- function(data, probs) {
  if (!is.numeric(probs) || length(probs) != 2L ||
      probs[1L] < 0 || probs[2L] > 1 || probs[1L] >= probs[2L]) {
    stop("'probs' must contain two ordered probabilities.", call. = FALSE)
  }
  id_columns <- setdiff(names(data), c(".draw", "estimate"))
  key <- do.call(
    interaction,
    c(data[id_columns], list(drop = TRUE, lex.order = TRUE))
  )
  groups <- split(seq_len(nrow(data)), key, drop = TRUE)
  out <- lapply(groups, function(rows) {
    identifiers <- data[rows[1L], id_columns, drop = FALSE]
    values <- data$estimate[rows]
    cbind(
      identifiers,
      data.frame(
        estimate = stats::median(values),
        mean = mean(values),
        median = stats::median(values),
        lower = unname(stats::quantile(values, probs[1L])),
        upper = unname(stats::quantile(values, probs[2L])),
        stringsAsFactors = FALSE
      )
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

validate_prediction_probs <- function(probs) {
  if (!is.numeric(probs) || length(probs) != 2L || any(!is.finite(probs)) ||
      probs[1L] < 0 || probs[2L] > 1 || probs[1L] >= probs[2L]) {
    stop("'probs' must contain two ordered probabilities.", call. = FALSE)
  }
  as.numeric(probs)
}

trajectory_predictions <- function(parameters, times, probs, summary, scale,
                                   reference_value, observation_sd = NULL) {
  identifier_columns <- setdiff(
    names(parameters),
    c(".draw", .epikinetics_parameters)
  )
  key <- do.call(
    interaction,
    c(parameters[identifier_columns], list(drop = TRUE, lex.order = TRUE))
  )
  groups <- split(seq_len(nrow(parameters)), key, drop = TRUE)

  out <- lapply(groups, function(rows) {
    values <- parameters[rows, , drop = FALSE]
    n_draws <- nrow(values)
    draw_rows <- rep(seq_len(n_draws), times = length(times))
    prediction_time <- rep(times, each = n_draws)
    estimate <- kinetics_mean(
      prediction_time,
      values$baseline[draw_rows],
      values$time_to_peak[draw_rows],
      values$time_to_peak[draw_rows] + values$waning_duration[draw_rows],
      values$boost_rate[draw_rows],
      values$early_waning_rate[draw_rows],
      values$late_waning_rate[draw_rows]
    )
    if (!is.null(observation_sd)) {
      estimate <- estimate + stats::rnorm(
        length(estimate),
        sd = observation_sd[match(values$.draw[draw_rows],
                                  names(observation_sd))]
      )
    }
    if (scale == "response") estimate <- reference_value * 2^estimate

    if (!summary) {
      result <- values[draw_rows, identifier_columns, drop = FALSE]
      result$.draw <- values$.draw[draw_rows]
      result$time <- prediction_time
      result$estimate <- estimate
      return(result)
    }

    estimate_matrix <- matrix(
      estimate,
      nrow = n_draws,
      ncol = length(times)
    )
    identifiers <- values[rep(1L, length(times)), identifier_columns,
                          drop = FALSE]
    medians <- matrixStats::colMedians(estimate_matrix)
    quantiles <- matrixStats::colQuantiles(
      estimate_matrix,
      probs = probs,
      drop = FALSE
    )
    identifiers$time <- times
    identifiers$estimate <- medians
    identifiers$mean <- colMeans(estimate_matrix)
    identifiers$median <- medians
    identifiers$lower <- quantiles[, 1L]
    identifiers$upper <- quantiles[, 2L]
    identifiers
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

prediction_censoring_limits <- function(prepared, scale, biomarkers) {
  data <- prepared$data
  data <- data[as.character(data$biomarker) %in% biomarkers, , drop = FALSE]
  response_from_model <- function(value) {
    prepared$specification$reference_value * 2^value
  }
  make_limits <- function(bound) {
    natural_column <- paste0(bound, "_limit")
    model_column <- paste0(bound, "_limit_model")
    present <- !is.na(data[[natural_column]])
    if (!any(present)) return(NULL)
    value <- if (scale == "model") {
      data[[model_column]][present]
    } else if (prepared$specification$scale == "natural") {
      data[[natural_column]][present]
    } else {
      response_from_model(data[[model_column]][present])
    }
    unique(data.frame(
      biomarker = factor(
        as.character(data$biomarker[present]),
        levels = prepared$mappings$biomarkers
      ),
      bound = bound,
      value = as.numeric(value),
      stringsAsFactors = FALSE
    ))
  }
  limits <- Filter(Negate(is.null), lapply(c("lower", "upper"), make_limits))
  if (!length(limits)) {
    return(data.frame(
      biomarker = factor(levels = prepared$mappings$biomarkers),
      bound = character(),
      value = numeric()
    ))
  }
  do.call(rbind, limits)
}

prediction_observations <- function(prepared, scale, participants, biomarkers) {
  data <- prepared$data
  keep <- as.character(data$biomarker) %in% biomarkers
  if (!is.null(participants)) keep <- keep & data$participant %in% participants
  data <- data[keep, , drop = FALSE]
  data$.plot_value <- if (scale == "model") {
    data$value_model
  } else if (prepared$specification$scale == "natural") {
    data$value
  } else {
    prepared$specification$reference_value * 2^data$value_model
  }
  data$biomarker <- factor(
    as.character(data$biomarker),
    levels = prepared$mappings$biomarkers
  )
  data
}

#' Predict biomarker trajectories
#'
#' @param object An `epikinetics_fit` object.
#' @param newdata Participant-level covariate profiles for population or new-
#'   participant predictions. `NULL` uses [prediction_grid()]: observed
#'   categorical combinations with continuous predictors fixed at their
#'   participant-level medians. The resulting predictions are conditional,
#'   not marginalised over the fitted covariate distribution.
#' @param times Non-negative times since exposure.
#' @param type `"population"` excludes participant variation; `"individual"`
#'   uses posterior effects for fitted participants; and `"new"` draws new
#'   participant effects for each posterior draw/profile. `"participant"` is
#'   retained as an alias for `"individual"`.
#' @param participants Optional fitted participant ids for individual
#'   prediction. `NULL` selects all fitted participants.
#' @param biomarkers Optional biomarker subset. The stored biomarker order is
#'   retained.
#' @param summary Return posterior summaries rather than individual draws.
#' @param ndraws Optional maximum number of posterior draws.
#' @param chunk_size Number of fitted participants processed together. Summary
#'   predictions are calculated in bounded chunks and do not materialise the
#'   complete participant-by-draw-by-time table.
#' @param max_rows Safety limit for unsummarised output. Subset participants,
#'   biomarkers, times, or posterior draws to stay below this value; use `Inf`
#'   only when the resulting memory requirement has been considered explicitly.
#' @param probs Lower and upper interval probabilities.
#' @param scale Return values on the natural response or model log2 scale.
#' @param include_observation_noise Include residual measurement error.
#' @param seed Optional seed for new-participant effects or observation noise.
#' @param ... Reserved for future methods.
#'
#' @return An `epikinetics_prediction` data frame. Unsummarised predictions
#'   include `.draw`; summaries include `estimate` (an alias for `median`),
#'   `mean`, `median`, `lower`, and `upper`.
#'   Without observation noise, intervals describe uncertainty in the latent
#'   expected trajectory. With observation noise, they are posterior predictive
#'   intervals for a future measurement.
#' @export
predict.epikinetics_fit <- function(
    object,
    newdata = NULL,
    times = 0:150,
    type = c("population", "individual", "participant", "new"),
    participants = NULL,
    biomarkers = NULL,
    summary = TRUE,
    ndraws = NULL,
    chunk_size = 20L,
    max_rows = 5e6,
    probs = c(0.025, 0.975),
    scale = c("response", "model"),
    include_observation_noise = FALSE,
    seed = NULL,
    ...) {
  type <- match.arg(type)
  if (type == "participant") type <- "individual"
  scale <- match.arg(scale)
  probs <- validate_prediction_probs(probs)
  if (!is.numeric(times) || !length(times) || any(!is.finite(times)) ||
      any(times < 0)) {
    stop("'times' must be a non-empty vector of finite non-negative values.",
         call. = FALSE)
  }
  times <- sort(unique(as.numeric(times)))
  if (!is.logical(summary) || length(summary) != 1L || is.na(summary) ||
      !is.logical(include_observation_noise) ||
      length(include_observation_noise) != 1L ||
      is.na(include_observation_noise)) {
    stop("'summary' and 'include_observation_noise' must be TRUE or FALSE.",
         call. = FALSE)
  }
  if (type == "individual" && !is.null(newdata)) {
    stop("'newdata' is not used for fitted-participant predictions.",
         call. = FALSE)
  }
  if (type == "individual" && !is.null(participants) &&
      (!length(participants) || anyNA(participants) ||
       anyDuplicated(participants))) {
    stop("'participants' must be NULL or a non-empty vector of unique, ",
         "non-missing fitted participant ids.", call. = FALSE)
  }
  chunk_size <- validate_count(chunk_size, "chunk_size")
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
      max_rows <= 0) {
    stop("'max_rows' must be one positive number or Inf.", call. = FALSE)
  }
  warn_if_pathological_sampling(object)
  draw_ids <- select_draw_ids(object, ndraws)
  biomarkers <- select_biomarkers(object$prepared, biomarkers)
  components <- if (type == "individual") {
    c("population", "participant_sd", "z", "beta")
  } else {
    c("population", "participant_sd", "beta")
  }
  if (include_observation_noise) {
    components <- c(components, "observation_sd")
  }
  matrices <- extract_kinetic_draw_matrices(
    object,
    draw_ids,
    components = components
  )
  observation_sd <- if (include_observation_noise) {
    stats::setNames(matrices$observation_sd, draw_ids)
  } else {
    NULL
  }
  prediction_metadata <- list()

  make_predictions <- function() {
    if (type != "individual") {
      parameters <- profile_parameter_draws(
        object,
        draw_ids,
        newdata = newdata,
        new_participant = type == "new",
        seed = NULL,
        matrices = matrices,
        biomarkers = biomarkers
      )
      metadata_names <- c(
        "prediction_grid", "grid_is_default", "categorical_rule",
        "continuous_rule"
      )
      parameter_attributes <- attributes(parameters)
      prediction_metadata <<- parameter_attributes[
        intersect(metadata_names, names(parameter_attributes))
      ]
      return(trajectory_predictions(
        parameters, times, probs, summary, scale,
        object$prepared$specification$reference_value,
        observation_sd
      ))
    }

    participant_levels <- object$prepared$mappings$participants
    if (is.null(participants)) participants <- participant_levels
    participant_indices <- match(participants, participant_levels)
    if (anyNA(participant_indices)) {
      unknown <- participants[is.na(participant_indices)]
      stop("Unknown participant(s): ",
           paste(utils::head(unknown, 5L), collapse = ", "), ".",
           call. = FALSE)
    }
    participants <- participant_levels[
      seq_along(participant_levels) %in% participant_indices
    ]
    requested_rows <- length(draw_ids) * length(participants) *
      length(biomarkers) * length(times)
    if (!summary && requested_rows > max_rows) {
      stop(
        "Unsummarised individual prediction would return ",
        format(requested_rows, big.mark = ","), " rows, above 'max_rows' (",
        format(max_rows, big.mark = ","), "). Subset participants, biomarkers, ",
        "times, or draws, or deliberately increase 'max_rows'.",
        call. = FALSE
      )
    }
    chunks <- split(
      participants,
      ceiling(seq_along(participants) / chunk_size)
    )
    result <- lapply(chunks, function(participant_chunk) {
      parameters <- participant_parameter_draws(
        object,
        draw_ids,
        participants = participant_chunk,
        derived = FALSE,
        matrices = matrices,
        biomarkers = biomarkers
      )
      trajectory_predictions(
        parameters, times, probs, summary, scale,
        object$prepared$specification$reference_value,
        observation_sd
      )
    })
    do.call(rbind, result)
  }

  out <- with_epikinetics_seed(seed, make_predictions())
  rownames(out) <- NULL
  out$biomarker <- as_ordered_biomarker(out$biomarker, object$prepared)
  class(out) <- c("epikinetics_prediction", "data.frame")
  attr(out, "type") <- type
  attr(out, "scale") <- scale
  attr(out, "summarised") <- summary
  attr(out, "uncertainty") <- if (include_observation_noise) {
    "posterior predictive observation"
  } else {
    "latent expected trajectory"
  }
  attr(out, "probs") <- probs
  attr(out, "central") <- "median"
  attr(out, "biomarker_order") <- object$prepared$mappings$biomarkers
  attr(out, "censoring_limits") <- prediction_censoring_limits(
    object$prepared, scale, biomarkers
  )
  if (type == "individual") {
    attr(out, "observations") <- prediction_observations(
      object$prepared,
      scale,
      unique(out$participant),
      biomarkers
    )
  }
  attr(out, "formula") <- object$prepared$specification$formula
  attr(out, "formula_variables") <- all.vars(
    object$prepared$specification$formula
  )
  attr(out, "categorical_covariates") <- if (type == "individual") {
    character()
  } else {
    object$prepared$specification$categorical_variables
  }
  attr(out, "continuous_covariates") <- if (type == "individual") {
    character()
  } else {
    object$prepared$specification$continuous_variables
  }
  for (name in names(prediction_metadata)) {
    attr(out, name) <- prediction_metadata[[name]]
  }
  out
}
