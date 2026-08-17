#' Extract raw posterior draws
#'
#' @param x An `epikinetics_fit` object.
#' @param variables Optional Stan variable names. Base names select all indexed
#'   elements, as in CmdStanR.
#' @param format One of the posterior package draw formats.
#'
#' @return A posterior draws object.
#' @export
posterior_draws <- function(
    x,
    variables = NULL,
    format = c("draws_df", "draws_matrix", "draws_array", "draws_list")) {
  if (!inherits(x, "epikinetics_fit")) {
    stop("'x' must be an epikinetics_fit object.", call. = FALSE)
  }
  format <- match.arg(format)

  if (inherits(x$fit, "CmdStanFit")) {
    return(x$fit$draws(variables = variables, format = format))
  }
  if (!posterior::is_draws(x$fit)) {
    stop("The fit object does not contain CmdStanR or posterior draws.",
         call. = FALSE)
  }

  draws <- x$fit
  if (!is.null(variables)) {
    available <- posterior::variables(draws)
    selected <- unique(unlist(lapply(variables, function(variable) {
      available[
        available == variable |
          startsWith(available, paste0(variable, "["))
      ]
    })))
    if (!length(selected)) {
      stop("None of the requested variables are present in the posterior.",
           call. = FALSE)
    }
    draws <- posterior::subset_draws(draws, variable = selected)
  }

  switch(
    format,
    draws_df = posterior::as_draws_df(draws),
    draws_matrix = posterior::as_draws_matrix(draws),
    draws_array = posterior::as_draws_array(draws),
    draws_list = posterior::as_draws_list(draws)
  )
}

draw_count <- function(x) {
  posterior::ndraws(posterior_draws(
    x,
    variables = "observation_sd",
    format = "draws_array"
  ))
}

select_draw_ids <- function(x, ndraws = NULL) {
  total <- draw_count(x)
  if (is.null(ndraws)) {
    return(seq_len(total))
  }
  ndraws <- validate_count(ndraws, "ndraws")
  if (ndraws >= total) {
    return(seq_len(total))
  }
  unique(as.integer(round(seq(1, total, length.out = ndraws))))
}

extract_indexed_matrix <- function(draws, variable, size) {
  if (size == 0L) {
    return(matrix(numeric(), nrow = nrow(draws), ncol = 0L))
  }
  expected <- paste0(variable, "[", seq_len(size), "]")
  missing <- setdiff(expected, colnames(draws))
  if (length(missing)) {
    stop("Posterior is missing expected variable(s): ",
         paste(utils::head(missing, 5L), collapse = ", "), ".",
         call. = FALSE)
  }
  unname(draws[, expected, drop = FALSE])
}

extract_kinetic_draw_matrices <- function(
    x,
    draw_ids,
    components = c(
      "population", "participant_sd", "z", "beta", "observation_sd"
    )) {
  allowed_components <- c(
    "population", "participant_sd", "z", "beta", "observation_sd"
  )
  unknown_components <- setdiff(components, allowed_components)
  if (length(unknown_components) || anyDuplicated(components)) {
    stop("Invalid posterior matrix component request.", call. = FALSE)
  }
  n_biomarkers <- length(x$prepared$mappings$biomarkers)
  n_participants <- length(x$prepared$mappings$participants)
  n_covariates <- length(x$prepared$mappings$covariates)
  active_covariate_parameters <- x$prepared$mappings$covariate_parameters
  if (is.null(active_covariate_parameters)) {
    active_covariate_parameters <- .epikinetics_parameters
  }
  active_participant_parameters <-
    x$prepared$mappings$participant_parameters
  if (is.null(active_participant_parameters)) {
    active_participant_parameters <- .epikinetics_parameters
  }

  variables <- character()
  if ("population" %in% components) {
    variables <- c(variables, unname(.population_prefixes))
  }
  if ("participant_sd" %in% components) {
    variables <- c(variables, unname(.participant_sd_prefixes))
  }
  if ("z" %in% components) {
    variables <- c(
      variables,
      unname(.z_prefixes[names(.z_prefixes) %in% active_participant_parameters])
    )
  }
  if ("beta" %in% components && n_covariates) {
    variables <- c(
      variables,
      unname(.beta_prefixes[names(.beta_prefixes) %in%
                              active_covariate_parameters])
    )
  }
  if ("observation_sd" %in% components) {
    variables <- c(variables, "observation_sd")
  }
  draws <- posterior_draws(
    x,
    variables = unique(variables),
    format = "draws_matrix"
  )
  draws <- draws[draw_ids, , drop = FALSE]
  result <- list()
  if ("population" %in% components) {
    result$population <- lapply(.population_prefixes, function(variable) {
      extract_indexed_matrix(draws, variable, n_biomarkers)
    })
  }
  if ("participant_sd" %in% components) {
    result$participant_sd <- lapply(
      .participant_sd_prefixes,
      function(variable) extract_indexed_matrix(draws, variable, n_biomarkers)
    )
  }
  if ("z" %in% components) {
    result$z <- lapply(names(.z_prefixes), function(parameter) {
      if (!parameter %in% active_participant_parameters) {
        return(matrix(0, nrow = length(draw_ids), ncol = n_participants))
      }
      extract_indexed_matrix(
        draws, .z_prefixes[[parameter]], n_participants
      )
    }) |>
      stats::setNames(names(.z_prefixes))
  }
  if ("beta" %in% components) {
    result$beta <- lapply(names(.beta_prefixes), function(parameter) {
      if (!n_covariates || !parameter %in% active_covariate_parameters) {
        return(matrix(0, nrow = length(draw_ids), ncol = n_covariates))
      }
      extract_indexed_matrix(
        draws,
        .beta_prefixes[[parameter]],
        n_covariates
      )
    }) |>
      stats::setNames(names(.beta_prefixes))
  }
  if ("observation_sd" %in% components) {
    if (!"observation_sd" %in% colnames(draws)) {
      stop("Posterior is missing expected variable 'observation_sd'.",
           call. = FALSE)
    }
    result$observation_sd <- unname(draws[, "observation_sd"])
  }
  result
}

add_derived_parameters <- function(data, reference_value) {
  data$waning_change_time <- data$time_to_peak + data$waning_duration
  data$peak_model <- data$baseline + data$boost_rate * data$time_to_peak
  data$waning_change_model <- data$peak_model -
    data$early_waning_rate * data$waning_duration
  data$baseline_response <- reference_value * 2^data$baseline
  data$peak_response <- reference_value * 2^data$peak_model
  data$waning_change_response <- reference_value * 2^data$waning_change_model
  data$early_waning_half_life <- 1 / data$early_waning_rate
  data$late_waning_half_life <- 1 / data$late_waning_rate
  data
}

select_biomarkers <- function(prepared, biomarkers = NULL) {
  available <- prepared$mappings$biomarkers
  if (is.null(biomarkers)) return(available)
  if (!is.character(biomarkers) || !length(biomarkers) || anyNA(biomarkers) ||
      any(!nzchar(biomarkers))) {
    stop("'biomarkers' must be NULL or a non-empty character vector.",
         call. = FALSE)
  }
  if (anyDuplicated(biomarkers)) {
    stop("'biomarkers' must not contain duplicates.", call. = FALSE)
  }
  unknown <- setdiff(biomarkers, available)
  if (length(unknown)) {
    stop("Unknown biomarker(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  available[available %in% biomarkers]
}

as_ordered_biomarker <- function(x, prepared) {
  factor(as.character(x), levels = prepared$mappings$biomarkers)
}

population_parameter_draws <- function(x, draw_ids, derived = TRUE) {
  matrices <- extract_kinetic_draw_matrices(
    x, draw_ids, components = "population"
  )$population
  biomarkers <- x$prepared$mappings$biomarkers
  out <- lapply(seq_along(biomarkers), function(k) {
    data.frame(
      .draw = draw_ids,
      biomarker = biomarkers[k],
      baseline = matrices$baseline[, k],
      time_to_peak = matrices$time_to_peak[, k],
      waning_duration = matrices$waning_duration[, k],
      boost_rate = matrices$boost_rate[, k],
      early_waning_rate = matrices$early_waning_rate[, k],
      late_waning_rate = matrices$late_waning_rate[, k],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out$biomarker <- as_ordered_biomarker(out$biomarker, x$prepared)
  if (derived) {
    out <- add_derived_parameters(
      out,
      x$prepared$specification$reference_value
    )
  }
  out
}

participant_parameter_draws <- function(x, draw_ids, participants = NULL,
                                        derived = TRUE, matrices = NULL,
                                        biomarkers = NULL) {
  participant_levels <- x$prepared$mappings$participants
  if (is.null(participants)) {
    participant_indices <- seq_along(participant_levels)
  } else {
    participant_indices <- match(participants, participant_levels)
    if (anyNA(participant_indices)) {
      unknown <- participants[is.na(participant_indices)]
      stop("Unknown participant(s): ",
           paste(utils::head(unknown, 5L), collapse = ", "), ".",
           call. = FALSE)
    }
  }

  all_matrices <- if (is.null(matrices)) {
    extract_kinetic_draw_matrices(
      x,
      draw_ids,
      components = c("population", "participant_sd", "z", "beta")
    )
  } else {
    matrices
  }
  population <- all_matrices$population
  participant_sd <- all_matrices$participant_sd
  z <- all_matrices$z
  beta <- all_matrices$beta
  X <- x$prepared$stan_data$X
  biomarkers <- select_biomarkers(x$prepared, biomarkers)
  biomarker_indices <- match(biomarkers, x$prepared$mappings$biomarkers)

  out <- vector("list", length(participant_indices) * length(biomarkers))
  position <- 0L
  for (participant_index in participant_indices) {
    covariate_effect <- lapply(.epikinetics_parameters, function(parameter) {
      if (!ncol(X)) {
        return(rep(0, length(draw_ids)))
      }
      as.numeric(beta[[parameter]] %*% X[participant_index, ])
    }) |>
      stats::setNames(.epikinetics_parameters)

    for (j in seq_along(biomarkers)) {
      k <- biomarker_indices[j]
      position <- position + 1L
      baseline <- population$baseline[, k] + covariate_effect$baseline +
        participant_sd$baseline[, k] * z$baseline[, participant_index]
      positive <- lapply(.epikinetics_positive_parameters, function(parameter) {
        population[[parameter]][, k] * exp(
          covariate_effect[[parameter]] +
            participant_sd[[parameter]][, k] * z[[parameter]][, participant_index]
        )
      }) |>
        stats::setNames(.epikinetics_positive_parameters)

      out[[position]] <- data.frame(
        .draw = draw_ids,
        participant = participant_levels[participant_index],
        biomarker = biomarkers[j],
        baseline = baseline,
        time_to_peak = positive$time_to_peak,
        waning_duration = positive$waning_duration,
        boost_rate = positive$boost_rate,
        early_waning_rate = positive$early_waning_rate,
        late_waning_rate = positive$late_waning_rate,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out$biomarker <- as_ordered_biomarker(out$biomarker, x$prepared)

  covariates <- setdiff(names(x$prepared$participant_data), ".participant")
  if (length(covariates)) {
    participant_rows <- match(
      out$participant,
      x$prepared$participant_data$.participant
    )
    out[covariates] <- x$prepared$participant_data[
      participant_rows,
      covariates,
      drop = FALSE
    ]
  }
  if (derived) {
    out <- add_derived_parameters(
      out,
      x$prepared$specification$reference_value
    )
  }
  out
}

regression_parameter_draws <- function(x, draw_ids) {
  covariates <- x$prepared$mappings$covariates
  metadata <- x$prepared$mappings$design_columns
  active_parameters <- x$prepared$mappings$covariate_parameters
  if (is.null(active_parameters)) active_parameters <- .epikinetics_parameters
  if (!length(covariates) || !length(active_parameters)) {
    return(data.frame(
      .draw = integer(),
      covariate = character(),
      term = character(),
      design_column = character(),
      level = character(),
      reference_level = character(),
      parameter = character(),
      effect_scale = character(),
      coefficient = numeric(),
      multiplicative_effect = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  beta <- extract_kinetic_draw_matrices(
    x, draw_ids, components = "beta"
  )$beta
  out <- vector("list", length(covariates) * length(active_parameters))
  position <- 0L
  for (parameter in active_parameters) {
    for (p in seq_along(covariates)) {
      position <- position + 1L
      coefficient <- beta[[parameter]][, p]
      out[[position]] <- data.frame(
        .draw = draw_ids,
        covariate = metadata$label[p],
        term = metadata$term[p],
        design_column = covariates[p],
        level = metadata$level[p],
        reference_level = metadata$reference_level[p],
        parameter = parameter,
        effect_scale = if (parameter == "baseline") {
          "additive on log2 response; multiplicative on response"
        } else {
          "additive on log parameter; multiplicative on parameter"
        },
        coefficient = coefficient,
        multiplicative_effect = if (parameter == "baseline") {
          2^coefficient
        } else {
          exp(coefficient)
        },
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

summarise_parameter_draws <- function(data, id_columns, probs) {
  if (!nrow(data)) {
    return(data)
  }
  if (!is.numeric(probs) || length(probs) != 2L || any(!is.finite(probs)) ||
      probs[1L] < 0 || probs[2L] > 1 || probs[1L] >= probs[2L]) {
    stop("'probs' must contain two ordered probabilities.", call. = FALSE)
  }
  measure_columns <- setdiff(
    names(data)[vapply(data, is.numeric, logical(1))],
    c(".draw", id_columns)
  )
  key_data <- lapply(data[id_columns], function(value) {
    if (anyNA(value)) {
      value <- as.character(value)
      value[is.na(value)] <- "<NA>"
    }
    value
  })
  key <- if (length(id_columns)) {
    do.call(interaction, c(key_data, list(drop = TRUE, lex.order = TRUE)))
  } else {
    factor(rep("all", nrow(data)))
  }
  groups <- split(seq_len(nrow(data)), key, drop = TRUE)
  measure_label <- if ("parameter" %in% id_columns) "quantity" else "parameter"

  out <- lapply(groups, function(rows) {
    identifiers <- if (length(id_columns)) data[rows[1L], id_columns, drop = FALSE]
    else data.frame(.placeholder = 1)[FALSE, , drop = FALSE]
    summaries <- lapply(measure_columns, function(parameter) {
      values <- data[[parameter]][rows]
      summary <- data.frame(
        measure = parameter,
        mean = mean(values),
        median = stats::median(values),
        sd = stats::sd(values),
        lower = unname(stats::quantile(values, probs[1L])),
        upper = unname(stats::quantile(values, probs[2L])),
        stringsAsFactors = FALSE
      )
      names(summary)[1L] <- measure_label
      summary
    })
    summary <- do.call(rbind, summaries)
    if (length(id_columns)) {
      identifiers[rep(1L, nrow(summary)), , drop = FALSE] |>
        cbind(summary)
    } else {
      summary
    }
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

#' Extract labelled kinetic parameters
#'
#' @param x An `epikinetics_fit` object.
#' @param level One of `"population"`, `"profile"`, `"participant"`, or
#'   `"regression"`. Population output is the biomarker-specific model
#'   intercept. Profile output applies the fitted covariate effects to the
#'   conditional profiles from [prediction_grid()] or `newdata`, but excludes
#'   participant random effects.
#' @param summary If `TRUE`, return means, medians, SDs, and intervals. If
#'   `FALSE`, return one labelled row per posterior draw.
#' @param newdata Optional participant-level covariate profiles when
#'   `level = "profile"`. `NULL` uses [prediction_grid()].
#' @param participants Optional participant ids when `level = "participant"`.
#' @param ndraws Optional maximum number of posterior draws to use.
#' @param probs Lower and upper interval probabilities.
#' @param derived Include peak values, transition values, and waning half-lives.
#'
#' @return A tidy data frame. Summaries use one row per parameter and group;
#'   unsummarised results retain parameter columns and `.draw`. Regression
#'   output includes the original formula term, design-column name, and decoded
#'   treatment level/reference when the contrast has that interpretation.
#' @export
posterior_parameters <- function(
    x,
    level = c("population", "profile", "participant", "regression"),
    summary = TRUE,
    newdata = NULL,
    participants = NULL,
    ndraws = NULL,
    probs = c(0.025, 0.975),
    derived = TRUE) {
  if (!inherits(x, "epikinetics_fit")) {
    stop("'x' must be an epikinetics_fit object.", call. = FALSE)
  }
  level <- match.arg(level)
  if (!is.logical(summary) || length(summary) != 1L || is.na(summary) ||
      !is.logical(derived) || length(derived) != 1L || is.na(derived)) {
    stop("'summary' and 'derived' must each be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(newdata) && level != "profile") {
    stop("'newdata' is only used when level = 'profile'.", call. = FALSE)
  }
  draw_ids <- select_draw_ids(x, ndraws)

  if (level == "population") {
    out <- population_parameter_draws(x, draw_ids, derived)
    id_columns <- "biomarker"
  } else if (level == "profile") {
    out <- profile_parameter_draws(
      x,
      draw_ids,
      newdata = newdata,
      new_participant = FALSE
    )
    if (derived) {
      out <- add_derived_parameters(
        out,
        x$prepared$specification$reference_value
      )
    }
    id_columns <- c(
      ".profile",
      "biomarker",
      all.vars(x$prepared$specification$formula)
    )
  } else if (level == "participant") {
    out <- participant_parameter_draws(x, draw_ids, participants, derived)
    id_columns <- c(
      "participant",
      "biomarker",
      setdiff(names(x$prepared$participant_data), ".participant")
    )
  } else {
    out <- regression_parameter_draws(x, draw_ids)
    id_columns <- c(
      "covariate", "term", "design_column", "level", "reference_level",
      "parameter", "effect_scale"
    )
  }

  if (summary) {
    out <- summarise_parameter_draws(out, id_columns, probs)
  }
  rownames(out) <- NULL
  out
}
