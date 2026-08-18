#' Validate and prepare data for epikinetics
#'
#' Converts an ordinary data frame into the complete, participant-indexed
#' representation used by the Stan model. This is a separate public operation
#' from [fit_epikinetics()] so that transformations, censoring, indices,
#' mappings, and the design matrix can be inspected before compilation or
#' sampling.
#'
#' The model assumes one focal exposure per participant. Covariates in
#' `formula` must likewise be constant within participant. Standard R contrast
#' handling is used: include the formula intercept (for example `~ age + sex`),
#' and epikinetics removes that intercept because biomarker-specific population
#' parameters already provide it.
#'
#' @param data A `data.frame` or object inheriting from it.
#' @param formula One-sided R formula for participant-level covariates. Numeric
#'   variables remain on their supplied scale; factors use the active R
#'   contrasts. Use `~ 1` for no covariates.
#' @param covariate_parameters Kinetic parameters modified by the formula.
#'   The default, `"all"`, applies the same design matrix to all six parameters,
#'   matching the original model. Supply any subset of `baseline`,
#'   `time_to_peak`, `waning_duration`, `boost_rate`, `early_waning_rate`, and
#'   `late_waning_rate` for a more explicit regression specification.
#' @param participant_parameters Kinetic parameters with participant-level
#'   random effects. The default allows participants to differ in `baseline`,
#'   `boost_rate`, `early_waning_rate`, and `late_waning_rate`, while sharing
#'   `time_to_peak` and `waning_duration` after conditioning on any selected
#'   covariate effects. In other words, those timing parameters have no residual
#'   participant random effect by default. This is a modelling assumption suited
#'   to the package's motivating data, not a universal biological constraint.
#'   Supply any parameter subset, or `"all"`, for another justified hierarchy.
#' @param id,time,exposure,biomarker,value Column names identifying participant,
#'   observation time, focal exposure time, biomarker type, and measurement.
#' @param biomarker_order Optional complete ordering of observed biomarker
#'   labels. If omitted, existing factor levels are preserved; otherwise the
#'   order of first appearance in `data` is used. The order is stored in the
#'   prepared object and propagated to posterior output, predictions, and plots.
#' @param scale Either `"natural"` (positive values transformed with base-2
#'   logarithms) or `"log2"` (values already on the model scale).
#' @param reference_value Positive reference used for natural-scale data:
#'   `log2(value / reference_value)`.
#' @param lower_limit,upper_limit Optional censoring limits. Each may be a
#'   scalar, a numeric vector with one value per row, a named numeric vector by
#'   biomarker, or the name of a numeric column. Use `NA` for no limit on a row.
#' @param censoring Optional censoring indicator or column name. Accepted values
#'   are `"none"`, `"left"`, and `"right"` (or `0`, `-1`, and `1`). When
#'   omitted, measurements at or beyond supplied limits are classified
#'   automatically.
#' @param priors An [epikinetics_priors()] object.
#'
#' @return An `epikinetics_data` object. Its public components include
#'   `input_data`, `observations`, `participants`, `model_frame`,
#'   `model_matrix`, `mappings`, and `stan_data`. `mappings$design_columns`
#'   links numeric design columns back to terms and treatment levels. Use
#'   [stan_data()] and `model.matrix()` as accessors.
#' @export
prepare_epikinetics_data <- function(
    data,
    formula = ~ 1,
    covariate_parameters = "all",
    participant_parameters = c(
      "baseline", "boost_rate", "early_waning_rate", "late_waning_rate"
    ),
    id = "pid",
    time = "day",
    exposure = "last_exp_day",
    biomarker = "titre_type",
    value = "value",
    biomarker_order = NULL,
    scale = c("natural", "log2"),
    reference_value = 1,
    lower_limit = NULL,
    upper_limit = NULL,
    censoring = NULL,
    priors = epikinetics_priors()) {
  scale <- match.arg(scale)
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or an object inheriting from it.",
         call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) {
    stop("'data' must contain at least one observation.", call. = FALSE)
  }

  columns <- c(id = id, time = time, exposure = exposure,
               biomarker = biomarker, value = value)
  if (any(lengths(columns) != 1L) || any(!nzchar(columns))) {
    stop("Column selectors must each be one non-empty column name.",
         call. = FALSE)
  }
  missing_columns <- setdiff(unname(columns), names(data))
  if (length(missing_columns)) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(unname(columns))) {
    stop("The id, time, exposure, biomarker, and value columns must be distinct.",
         call. = FALSE)
  }

  if (anyNA(data[[id]])) {
    stop("Participant IDs must not be missing.", call. = FALSE)
  }
  id_text <- trimws(as.character(data[[id]]))
  if (any(!nzchar(id_text))) {
    stop("Participant IDs must not be empty.", call. = FALSE)
  }
  if (anyNA(data[[biomarker]])) {
    stop("Biomarker labels must not be missing.", call. = FALSE)
  }

  required <- data[unname(columns)]
  if (anyNA(required)) {
    bad <- names(required)[vapply(required, anyNA, logical(1))]
    stop("Missing values are not allowed in required columns: ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.atomic(data[[id]]) || is.list(data[[id]])) {
    stop("The participant id column must be an atomic vector.", call. = FALSE)
  }
  if (!is.numeric(data[[value]]) || any(!is.finite(data[[value]]))) {
    stop("The measurement column '", value,
         "' must contain finite numeric values.", call. = FALSE)
  }
  if (scale == "natural" && any(data[[value]] <= 0)) {
    stop("Natural-scale measurements must all be positive.", call. = FALSE)
  }
  if (!is.numeric(reference_value) || length(reference_value) != 1L ||
      !is.finite(reference_value) || reference_value <= 0) {
    stop("'reference_value' must be one finite positive number.",
         call. = FALSE)
  }

  time_since_exposure <- difference_in_days(data[[time]], data[[exposure]])
  if (any(!is.finite(time_since_exposure))) {
    stop("Observation and exposure times must have a finite numeric difference.",
         call. = FALSE)
  }
  if (any(time_since_exposure < 0)) {
    stop("All observations must occur on or after the focal exposure.",
         call. = FALSE)
  }

  id_levels <- unique(data[[id]])
  participant_index <- match(data[[id]], id_levels)
  biomarker_levels <- resolve_biomarker_order(
    data[[biomarker]],
    biomarker_order
  )
  if (any(!nzchar(biomarker_levels))) {
    stop("Biomarker labels must be non-empty.", call. = FALSE)
  }
  biomarker_index <- match(as.character(data[[biomarker]]), biomarker_levels)

  validate_participant_constant(data[[exposure]], participant_index,
                                exposure, id_levels)

  design <- build_epikinetics_design(data, formula, participant_index,
                                     id_levels)
  covariate_parameters <- validate_covariate_parameters(
    covariate_parameters,
    ncol(design$matrix)
  )
  covariate_active <- as.integer(
    .epikinetics_parameters %in% covariate_parameters
  )
  participant_parameters <- validate_participant_parameters(
    participant_parameters
  )
  participant_effect_active <- as.integer(
    .epikinetics_parameters %in% participant_parameters
  )

  lower_input <- resolve_censoring_limit(
    lower_limit, data, biomarker_levels, biomarker_index, "lower_limit"
  )
  upper_input <- resolve_censoring_limit(
    upper_limit, data, biomarker_levels, biomarker_index, "upper_limit"
  )

  both_limits <- !is.na(lower_input) & !is.na(upper_input)
  if (any(lower_input[both_limits] >= upper_input[both_limits])) {
    stop("Every finite lower censoring limit must be below its upper limit.",
         call. = FALSE)
  }
  if (scale == "natural") {
    finite_limits <- c(lower_input[!is.na(lower_input)],
                       upper_input[!is.na(upper_input)])
    if (length(finite_limits) && any(finite_limits <= 0)) {
      stop("Natural-scale censoring limits must be positive.", call. = FALSE)
    }
  }

  censoring_code <- resolve_censoring(
    censoring, data, data[[value]], lower_input, upper_input
  )
  if (any(censoring_code == -1L & is.na(lower_input))) {
    stop("Each left-censored observation requires a lower limit.",
         call. = FALSE)
  }
  if (any(censoring_code == 1L & is.na(upper_input))) {
    stop("Each right-censored observation requires an upper limit.",
         call. = FALSE)
  }

  validate_censoring_consistency(
    value = data[[value]],
    censoring = censoring_code,
    lower_limit = lower_input,
    upper_limit = upper_input
  )

  transform_model_scale <- function(x) {
    if (scale == "natural") log2(x / reference_value) else x
  }
  value_model <- transform_model_scale(data[[value]])
  lower_model <- rep(0, nrow(data))
  upper_model <- rep(0, nrow(data))
  lower_model[!is.na(lower_input)] <- transform_model_scale(
    lower_input[!is.na(lower_input)]
  )
  upper_model[!is.na(upper_input)] <- transform_model_scale(
    upper_input[!is.na(upper_input)]
  )

  order_index <- order(participant_index, time_since_exposure,
                       biomarker_index, seq_len(nrow(data)))
  participant_index <- participant_index[order_index]
  biomarker_index <- biomarker_index[order_index]

  canonical <- data.frame(
    source_row = order_index,
    participant = data[[id]][order_index],
    participant_index = participant_index,
    observation_time = data[[time]][order_index],
    exposure_time = data[[exposure]][order_index],
    time_since_exposure = time_since_exposure[order_index],
    biomarker = factor(
      as.character(data[[biomarker]][order_index]),
      levels = biomarker_levels
    ),
    biomarker_index = biomarker_index,
    value = data[[value]][order_index],
    value_model = value_model[order_index],
    lower_limit = lower_input[order_index],
    lower_limit_model = lower_model[order_index],
    upper_limit = upper_input[order_index],
    upper_limit_model = upper_model[order_index],
    censoring = c("left", "none", "right")[censoring_code[order_index] + 2L],
    censoring_code = censoring_code[order_index],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  covariate_names <- all.vars(formula)
  if (length(covariate_names)) {
    canonical[covariate_names] <- data[order_index, covariate_names,
                                       drop = FALSE]
  }

  participant_counts <- tabulate(participant_index, nbins = length(id_levels))
  observation_end <- cumsum(participant_counts)
  observation_start <- observation_end - participant_counts + 1L

  stan_data <- c(
    list(
      N_observations = nrow(data),
      N_participants = length(id_levels),
      N_biomarkers = length(biomarker_levels),
      N_covariates = ncol(design$matrix),
      biomarker = as.integer(biomarker_index),
      time = as.numeric(time_since_exposure[order_index]),
      value = as.numeric(value_model[order_index]),
      censoring = as.integer(censoring_code[order_index]),
      lower_limit = as.numeric(lower_model[order_index]),
      upper_limit = as.numeric(upper_model[order_index]),
      observation_start = as.integer(observation_start),
      observation_end = as.integer(observation_end),
      participant_sequence = seq_along(id_levels),
      X = unname(design$matrix),
      covariate_active = covariate_active,
      participant_effect_active = participant_effect_active,
      grainsize = 1L
    ),
    priors_to_stan(priors)
  )

  validate_epikinetics_stan_data(stan_data)

  rownames(design$matrix) <- as.character(id_levels)
  first_rows <- match(seq_along(id_levels), match(data[[id]], id_levels))
  participants <- data.frame(
    participant_index = seq_along(id_levels),
    participant = id_levels,
    exposure_time = data[[exposure]][first_rows],
    observation_count = participant_counts,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  covariate_variables <- all.vars(formula)
  if (length(covariate_variables)) {
    participants[covariate_variables] <- design$participant_data[
      covariate_variables
    ]
  }
  biomarker_counts <- tabulate(
    biomarker_index,
    nbins = length(biomarker_levels)
  )
  participant_mapping <- participants[c(
    "participant_index", "participant", "observation_count"
  )]
  biomarker_mapping <- data.frame(
    biomarker_index = seq_along(biomarker_levels),
    biomarker = factor(biomarker_levels, levels = biomarker_levels),
    observation_count = biomarker_counts,
    stringsAsFactors = FALSE
  )
  reference_levels <- vapply(
    design$contrast_matrices,
    contrast_reference_level,
    character(1)
  )

  structure(
    list(
      input_data = data,
      observations = canonical,
      data = canonical,
      participants = participants,
      participant_data = design$participant_data,
      model_frame = design$model_frame,
      model_matrix = design$matrix,
      stan_data = stan_data,
      priors = priors,
      mappings = list(
        participants = id_levels,
        biomarkers = biomarker_levels,
        biomarker_order = biomarker_levels,
        covariates = colnames(design$matrix),
        covariate_parameters = covariate_parameters,
        participant_parameters = participant_parameters,
        participant = participant_mapping,
        biomarker = biomarker_mapping,
        factor_levels = design$xlevels,
        reference_levels = reference_levels,
        contrasts = design$contrasts,
        contrast_matrices = design$contrast_matrices,
        design_columns = design$column_metadata,
        variables = design$variable_metadata
      ),
      specification = list(
        formula = formula,
        covariate_parameters = covariate_parameters,
        participant_parameters = participant_parameters,
        biomarker_order = biomarker_levels,
        biomarker_order_source = if (!is.null(biomarker_order)) {
          "explicit"
        } else if (is.factor(data[[biomarker]])) {
          "factor levels"
        } else {
          "first appearance"
        },
        terms = design$terms,
        term_labels = attr(design$terms, "term.labels"),
        design_assignment = design$assignment,
        xlevels = design$xlevels,
        contrasts = design$contrasts,
        categorical_variables = design$categorical_variables,
        continuous_variables = design$continuous_variables,
        prediction_defaults = design$prediction_defaults,
        columns = as.list(columns),
        scale = scale,
        reference_value = reference_value,
        transformation = if (scale == "natural") {
          paste0("log2(value / ", format(reference_value), ")")
        } else {
          "values supplied on the log2 model scale"
        }
      )
    ),
    class = "epikinetics_data"
  )
}

validate_covariate_parameters <- function(x, n_covariates) {
  if (!is.character(x) || anyNA(x)) {
    stop("'covariate_parameters' must be 'all' or a character vector of ",
         "kinetic parameter names.", call. = FALSE)
  }
  if (identical(x, "all")) {
    x <- .epikinetics_parameters
  }
  if (!length(x)) {
    if (n_covariates > 0L) {
      stop("Select at least one 'covariate_parameters' value when the ",
           "formula contains covariates.", call. = FALSE)
    }
    return(character())
  }
  if (anyDuplicated(x)) {
    stop("'covariate_parameters' must not contain duplicates.", call. = FALSE)
  }
  unknown <- setdiff(x, .epikinetics_parameters)
  if (length(unknown)) {
    stop("Unknown covariate-effect parameter(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  if (n_covariates == 0L) character() else x
}

validate_participant_parameters <- function(x) {
  if (!is.character(x) || anyNA(x)) {
    stop("'participant_parameters' must be 'all' or a character vector of ",
         "kinetic parameter names.", call. = FALSE)
  }
  if (identical(x, "all")) {
    x <- .epikinetics_parameters
  }
  if (anyDuplicated(x)) {
    stop("'participant_parameters' must not contain duplicates.",
         call. = FALSE)
  }
  unknown <- setdiff(x, .epikinetics_parameters)
  if (length(unknown)) {
    stop("Unknown participant-effect parameter(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  x
}

resolve_biomarker_order <- function(biomarker, biomarker_order = NULL) {
  observed <- unique(as.character(biomarker))
  if (any(!nzchar(observed))) {
    stop("Biomarker labels must be non-empty.", call. = FALSE)
  }

  if (is.null(biomarker_order)) {
    if (is.factor(biomarker)) {
      return(levels(biomarker)[levels(biomarker) %in% observed])
    }
    return(observed)
  }

  if (!is.character(biomarker_order) || anyNA(biomarker_order) ||
      any(!nzchar(biomarker_order))) {
    stop("'biomarker_order' must be NULL or a character vector of non-empty ",
         "biomarker labels.", call. = FALSE)
  }
  if (anyDuplicated(biomarker_order)) {
    stop("'biomarker_order' must not contain duplicate labels.",
         call. = FALSE)
  }
  missing <- setdiff(observed, biomarker_order)
  extra <- setdiff(biomarker_order, observed)
  if (length(missing) || length(extra)) {
    details <- c(
      if (length(missing)) paste0("missing: ", paste(missing, collapse = ", ")),
      if (length(extra)) paste0("not observed: ", paste(extra, collapse = ", "))
    )
    stop("'biomarker_order' must contain every observed biomarker exactly ",
         "once (", paste(details, collapse = "; "), ").", call. = FALSE)
  }
  biomarker_order
}

difference_in_days <- function(time, exposure) {
  if (is.character(time) && is.character(exposure)) {
    parsed_time <- suppressWarnings(as.Date(time))
    parsed_exposure <- suppressWarnings(as.Date(exposure))
    if (!anyNA(parsed_time) && !anyNA(parsed_exposure)) {
      return(as.numeric(parsed_time - parsed_exposure))
    }
  }
  if (inherits(time, "Date") && inherits(exposure, "Date")) {
    return(as.numeric(time - exposure))
  }
  if (inherits(time, "POSIXt") && inherits(exposure, "POSIXt")) {
    return(as.numeric(difftime(time, exposure, units = "days")))
  }
  if (is.numeric(time) && is.numeric(exposure)) {
    return(as.numeric(time - exposure))
  }
  stop(
    "Observation and exposure times must both be numeric, Date, or POSIXt ",
    "vectors of the same kind.",
    call. = FALSE
  )
}

validate_participant_constant <- function(x, participant_index, variable,
                                          id_levels) {
  bad <- vapply(
    split(x, participant_index),
    function(values) length(unique(values)) != 1L,
    logical(1)
  )
  if (any(bad)) {
    labels <- id_levels[as.integer(names(bad)[bad])]
    stop(
      "'", variable, "' must be constant within participant. Problematic ",
      "participant(s): ", paste(utils::head(labels, 5L), collapse = ", "),
      if (sum(bad) > 5L) ", ..." else "", ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

build_epikinetics_design <- function(data, formula, participant_index,
                                     id_levels) {
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("'formula' must be a one-sided R formula.", call. = FALSE)
  }
  terms_object <- stats::terms(formula)
  if (attr(terms_object, "response") != 0L) {
    stop("'formula' must not contain a response.", call. = FALSE)
  }
  variables <- all.vars(formula)
  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables)) {
    stop("Unknown formula variable(s): ",
         paste(missing_variables, collapse = ", "), ".", call. = FALSE)
  }
  if (length(variables) && attr(terms_object, "intercept") == 0L) {
    stop(
      "Covariate formulas must include an intercept; use '~ covariate', not ",
      "'~ 0 + covariate'. epikinetics removes the redundant intercept ",
      "internally.",
      call. = FALSE
    )
  }

  for (variable in variables) {
    if (anyNA(data[[variable]])) {
      stop("Missing covariate values are not supported ('", variable, "').",
           call. = FALSE)
    }
    validate_participant_constant(data[[variable]], participant_index,
                                  variable, id_levels)
  }

  first_rows <- match(seq_along(id_levels), participant_index)
  participant_data <- data[first_rows, variables, drop = FALSE]
  for (variable in variables) {
    if (is.character(participant_data[[variable]])) {
      participant_data[[variable]] <- factor(participant_data[[variable]])
    }
  }
  participant_data$.participant <- id_levels
  participant_data <- participant_data[c(".participant", variables)]

  model_frame <- stats::model.frame(
    terms_object,
    data = participant_data,
    na.action = stats::na.fail,
    drop.unused.levels = FALSE
  )
  terms_object <- stats::terms(model_frame)
  matrix <- tryCatch(
    stats::model.matrix(terms_object, model_frame),
    error = function(error) {
      stop(
        "Could not construct the participant covariate design matrix: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  contrasts <- attr(matrix, "contrasts")
  assignment <- attr(matrix, "assign")
  keep <- assignment != 0L
  matrix <- matrix[, keep, drop = FALSE]
  assignment <- assignment[keep]
  storage.mode(matrix) <- "double"
  if (any(!is.finite(matrix))) {
    stop("The covariate design matrix must contain only finite values.",
         call. = FALSE)
  }
  if (ncol(matrix) && any(vapply(
    seq_len(ncol(matrix)),
    function(column) length(unique(matrix[, column])) < 2L,
    logical(1)
  ))) {
    constant <- colnames(matrix)[vapply(
      seq_len(ncol(matrix)),
      function(column) length(unique(matrix[, column])) < 2L,
      logical(1)
    )]
    stop(
      "Covariate design columns must vary across participants. Constant ",
      "column(s): ", paste(constant, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (ncol(matrix) && qr(matrix)$rank < ncol(matrix)) {
    stop(
      "The participant covariate design matrix is rank deficient. Remove ",
      "redundant covariates or factor levels.",
      call. = FALSE
    )
  }

  xlevels <- lapply(model_frame[vapply(model_frame, is.factor, logical(1))],
                    levels)
  contrast_matrices <- lapply(
    model_frame[vapply(model_frame, is.factor, logical(1))],
    stats::contrasts
  )
  categorical_variables <- variables[vapply(
    participant_data[variables],
    function(value) is.factor(value) || is.character(value) || is.logical(value),
    logical(1)
  )]
  continuous_variables <- setdiff(variables, categorical_variables)
  prediction_defaults <- lapply(
    participant_data[continuous_variables],
    stats::median
  )
  variable_metadata <- data.frame(
    variable = variables,
    type = ifelse(variables %in% categorical_variables,
                  "categorical", "continuous"),
    reference_level = vapply(variables, function(variable) {
      if (!variable %in% names(contrast_matrices)) return(NA_character_)
      contrast_reference_level(contrast_matrices[[variable]])
    }, character(1)),
    default = vapply(variables, function(variable) {
      if (variable %in% continuous_variables) {
        return(format(prediction_defaults[[variable]], trim = TRUE))
      }
      NA_character_
    }, character(1)),
    stringsAsFactors = FALSE
  )
  column_metadata <- design_column_metadata(
    matrix = matrix,
    assignment = assignment,
    terms = terms_object,
    xlevels = xlevels,
    contrast_matrices = contrast_matrices
  )
  list(
    matrix = matrix,
    participant_data = participant_data,
    model_frame = model_frame,
    terms = terms_object,
    xlevels = xlevels,
    contrasts = contrasts,
    contrast_matrices = contrast_matrices,
    assignment = assignment,
    categorical_variables = categorical_variables,
    continuous_variables = continuous_variables,
    prediction_defaults = prediction_defaults,
    variable_metadata = variable_metadata,
    column_metadata = column_metadata
  )
}

contrast_reference_level <- function(contrast_matrix) {
  if (is.null(contrast_matrix) || !length(contrast_matrix)) {
    return(NA_character_)
  }
  zero_rows <- rowSums(abs(contrast_matrix)) == 0
  if (sum(zero_rows) != 1L) return(NA_character_)
  rownames(contrast_matrix)[zero_rows]
}

design_column_metadata <- function(matrix, assignment, terms, xlevels,
                                   contrast_matrices) {
  term_labels <- attr(terms, "term.labels")
  if (!ncol(matrix)) {
    return(data.frame(
      design_column = character(), term = character(), variables = character(),
      level = character(), reference_level = character(), label = character(),
      stringsAsFactors = FALSE
    ))
  }
  term <- term_labels[assignment]
  term_variables <- vapply(term, function(label) {
    paste(all.vars(stats::as.formula(paste("~", label))), collapse = ", ")
  }, character(1))
  out <- data.frame(
    design_column = colnames(matrix),
    term = term,
    variables = unname(term_variables),
    level = NA_character_,
    reference_level = NA_character_,
    label = colnames(matrix),
    stringsAsFactors = FALSE
  )

  for (variable in intersect(names(xlevels), term_labels)) {
    rows <- which(out$term == variable)
    contrast_matrix <- contrast_matrices[[variable]]
    reference <- contrast_reference_level(contrast_matrix)
    if (!length(rows) || is.null(contrast_matrix) ||
        ncol(contrast_matrix) != length(rows) || is.na(reference)) {
      next
    }
    represented_level <- vapply(seq_len(ncol(contrast_matrix)), function(j) {
      values <- contrast_matrix[, j]
      candidates <- rownames(contrast_matrix)[
        abs(values - 1) < sqrt(.Machine$double.eps)
      ]
      if (length(candidates) == 1L &&
          all(abs(values[rownames(contrast_matrix) != candidates]) <
              sqrt(.Machine$double.eps))) {
        candidates
      } else {
        NA_character_
      }
    }, character(1))
    out$level[rows] <- represented_level
    out$reference_level[rows] <- reference
    decoded <- !is.na(represented_level)
    out$label[rows[decoded]] <- paste0(
      variable, "=", represented_level[decoded], " (vs ", reference, ")"
    )
  }
  out
}

resolve_censoring_limit <- function(limit, data, biomarker_levels,
                                    biomarker_index, argument) {
  if (is.null(limit)) {
    return(rep(NA_real_, nrow(data)))
  }
  if (is.character(limit) && length(limit) == 1L && limit %in% names(data)) {
    limit <- data[[limit]]
  } else if (is.numeric(limit) && !is.null(names(limit))) {
    missing <- setdiff(biomarker_levels, names(limit))
    if (length(missing)) {
      stop("Named '", argument, "' is missing biomarker(s): ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    limit <- unname(limit[biomarker_levels][biomarker_index])
  } else if (is.numeric(limit) && length(limit) == 1L) {
    limit <- rep(limit, nrow(data))
  }
  if (!is.numeric(limit) || length(limit) != nrow(data) ||
      any(!is.finite(limit[!is.na(limit)]))) {
    stop(
      "'", argument, "' must resolve to finite numeric values (or NA), one ",
      "per observation.",
      call. = FALSE
    )
  }
  as.numeric(limit)
}

resolve_censoring <- function(censoring, data, value, lower_limit, upper_limit) {
  if (is.null(censoring)) {
    code <- integer(length(value))
    code[!is.na(lower_limit) & value <= lower_limit] <- -1L
    code[!is.na(upper_limit) & value >= upper_limit] <- 1L
    return(code)
  }
  if (is.character(censoring) && length(censoring) == 1L &&
      censoring %in% names(data)) {
    censoring <- data[[censoring]]
  } else if (length(censoring) == 1L) {
    censoring <- rep(censoring, nrow(data))
  }
  if (length(censoring) != nrow(data)) {
    stop("'censoring' must contain one value per observation.", call. = FALSE)
  }
  if (is.numeric(censoring)) {
    if (anyNA(censoring) || any(!censoring %in% c(-1, 0, 1))) {
      stop("Numeric censoring values must be -1, 0, or 1.", call. = FALSE)
    }
    return(as.integer(censoring))
  }
  labels <- tolower(as.character(censoring))
  aliases <- c(none = 0L, uncensored = 0L, left = -1L, lower = -1L,
               right = 1L, upper = 1L)
  if (anyNA(labels) || any(!labels %in% names(aliases))) {
    stop("Censoring labels must be 'none', 'left', or 'right'.",
         call. = FALSE)
  }
  unname(aliases[labels])
}

validate_censoring_consistency <- function(value, censoring, lower_limit,
                                           upper_limit) {
  left_above <- censoring == -1L & value > lower_limit
  right_below <- censoring == 1L & value < upper_limit
  uncensored_at_or_below <- censoring == 0L & !is.na(lower_limit) &
    value <= lower_limit
  uncensored_at_or_above <- censoring == 0L & !is.na(upper_limit) &
    value >= upper_limit

  report_rows <- function(condition) {
    paste(utils::head(which(condition), 5L), collapse = ", ")
  }
  if (any(left_above)) {
    stop(
      "Left-censored observations must be at or below their lower_limit. ",
      "Problematic row(s): ", report_rows(left_above), ".",
      call. = FALSE
    )
  }
  if (any(right_below)) {
    stop(
      "Right-censored observations must be at or above their upper_limit. ",
      "Problematic row(s): ", report_rows(right_below), ".",
      call. = FALSE
    )
  }
  if (any(uncensored_at_or_below)) {
    stop(
      "Uncensored observations must be greater than their lower_limit. ",
      "Problematic row(s): ", report_rows(uncensored_at_or_below), ".",
      call. = FALSE
    )
  }
  if (any(uncensored_at_or_above)) {
    stop(
      "Uncensored observations must be less than their upper_limit. ",
      "Problematic row(s): ", report_rows(uncensored_at_or_above), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_epikinetics_stan_data <- function(data) {
  required <- c(
    "N_observations", "N_participants", "N_biomarkers", "N_covariates",
    "biomarker", "time", "value", "censoring", "lower_limit",
    "upper_limit", "observation_start", "observation_end",
    "participant_sequence", "X", "grainsize", "population_prior_mean",
    "covariate_active", "participant_effect_active",
    "population_prior_sd", "participant_sd_prior_scale",
    "covariate_prior_scale", "observation_sd_prior_scale"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Prepared Stan data are missing field(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }

  count <- function(name, allow_zero = FALSE) {
    value <- data[[name]]
    minimum <- if (allow_zero) 0L else 1L
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value != as.integer(value) || value < minimum) {
      stop("Stan data field '", name, "' must be one ",
           if (allow_zero) "non-negative" else "positive", " integer.",
           call. = FALSE)
    }
    as.integer(value)
  }
  n_observations <- count("N_observations")
  n_participants <- count("N_participants")
  n_biomarkers <- count("N_biomarkers")
  n_covariates <- count("N_covariates", allow_zero = TRUE)
  count("grainsize")

  observation_fields <- c(
    "biomarker", "time", "value", "censoring", "lower_limit", "upper_limit"
  )
  bad_length <- observation_fields[
    lengths(data[observation_fields]) != n_observations
  ]
  if (length(bad_length)) {
    stop("Stan observation field(s) have incorrect length: ",
         paste(bad_length, collapse = ", "), ".", call. = FALSE)
  }
  numeric_fields <- c(
    observation_fields, "observation_start", "observation_end",
    "participant_sequence", "population_prior_mean", "population_prior_sd",
    "covariate_active", "participant_effect_active",
    "participant_sd_prior_scale", "covariate_prior_scale",
    "observation_sd_prior_scale"
  )
  non_finite <- numeric_fields[vapply(
    data[numeric_fields],
    function(value) !is.numeric(value) || any(!is.finite(value)),
    logical(1)
  )]
  if (length(non_finite)) {
    stop("Stan data must be numeric and finite; problem in field(s): ",
         paste(non_finite, collapse = ", "), ".", call. = FALSE)
  }
  if (any(data$biomarker != as.integer(data$biomarker)) ||
      any(data$biomarker < 1L | data$biomarker > n_biomarkers)) {
    stop("Stan biomarker indices must be integers from 1 to N_biomarkers.",
         call. = FALSE)
  }
  if (any(data$time < 0)) {
    stop("Stan observation times must be non-negative.", call. = FALSE)
  }
  if (any(data$censoring != as.integer(data$censoring)) ||
      any(!data$censoring %in% c(-1L, 0L, 1L))) {
    stop("Stan censoring indicators must be -1, 0, or 1.", call. = FALSE)
  }
  if (length(data$observation_start) != n_participants ||
      length(data$observation_end) != n_participants ||
      length(data$participant_sequence) != n_participants) {
    stop("Participant index arrays must have length N_participants.",
         call. = FALSE)
  }
  if (!identical(as.integer(data$participant_sequence),
                 seq_len(n_participants))) {
    stop("Stan participant_sequence must be 1:N_participants.",
         call. = FALSE)
  }
  starts <- as.integer(data$observation_start)
  ends <- as.integer(data$observation_end)
  contiguous <- starts[1L] == 1L && ends[n_participants] == n_observations &&
    all(starts <= ends) &&
    (n_participants == 1L || all(starts[-1L] == ends[-n_participants] + 1L))
  if (!contiguous) {
    stop(
      "Stan participant observation ranges must be non-empty, contiguous, ",
      "and cover every observation exactly once.",
      call. = FALSE
    )
  }
  if (!is.matrix(data$X) ||
      !identical(dim(data$X), c(n_participants, n_covariates)) ||
      any(!is.finite(data$X))) {
    stop("Stan design matrix X must be a finite N_participants by ",
         "N_covariates matrix.", call. = FALSE)
  }
  if (length(data$covariate_active) != length(.epikinetics_parameters) ||
      any(data$covariate_active != as.integer(data$covariate_active)) ||
      any(!data$covariate_active %in% c(0L, 1L))) {
    stop("Stan covariate_active must contain six values equal to 0 or 1.",
         call. = FALSE)
  }
  if (n_covariates == 0L && any(data$covariate_active != 0L)) {
    stop("Stan covariate_active must be zero when N_covariates is zero.",
         call. = FALSE)
  }
  if (length(data$participant_effect_active) !=
      length(.epikinetics_parameters) ||
      any(data$participant_effect_active !=
          as.integer(data$participant_effect_active)) ||
      any(!data$participant_effect_active %in% c(0L, 1L))) {
    stop("Stan participant_effect_active must contain six values equal to ",
         "0 or 1.", call. = FALSE)
  }
  prior_vectors <- c(
    "population_prior_mean", "population_prior_sd",
    "participant_sd_prior_scale", "covariate_prior_scale"
  )
  if (any(lengths(data[prior_vectors]) != length(.epikinetics_parameters))) {
    stop("Every Stan kinetic prior vector must have length ",
         length(.epikinetics_parameters), ".", call. = FALSE)
  }
  positive_priors <- c(
    "population_prior_sd", "participant_sd_prior_scale",
    "covariate_prior_scale", "observation_sd_prior_scale"
  )
  if (any(vapply(data[positive_priors], function(value) any(value <= 0),
                 logical(1)))) {
    stop("All Stan prior scale values must be positive.", call. = FALSE)
  }
  invisible(data)
}

#' @export
print.epikinetics_data <- function(x, ...) {
  counts <- table(factor(
    x$observations$censoring,
    levels = c("none", "left", "right")
  ))
  cat("Prepared epikinetics model data\n")
  cat("  Observations: ", nrow(x$observations), "\n", sep = "")
  cat("  Participants: ", length(x$mappings$participants), "\n", sep = "")
  cat("  Biomarkers:   ", length(x$mappings$biomarkers), " (",
      paste(x$mappings$biomarkers, collapse = ", "), "; ",
      x$specification$biomarker_order_source, " order)\n", sep = "")
  formula_variables <- all.vars(x$specification$formula)
  cat("  Covariates:   ",
      if (length(formula_variables)) {
        paste(formula_variables, collapse = ", ")
      } else {
        "none"
      }, "\n", sep = "")
  if (length(x$mappings$covariates)) {
    cat("  Effects on:  ",
        paste(x$mappings$covariate_parameters, collapse = ", "),
        "\n", sep = "")
  }
  cat("  Random effects: ",
      if (length(x$mappings$participant_parameters)) {
        paste(x$mappings$participant_parameters, collapse = ", ")
      } else {
        "none"
      }, "\n", sep = "")
  cat("  Censoring:    ",
      paste(names(counts), as.integer(counts), sep = "=", collapse = ", "),
      "\n", sep = "")
  cat("  Time range:   ",
      paste(format(range(x$observations$time_since_exposure)), collapse = " to "),
      " since exposure\n", sep = "")
  cat("  Model scale:  ", x$specification$transformation, "; range ",
      paste(format(range(x$observations$value_model)), collapse = " to "),
      "\n", sep = "")
  cat("  Exposure:     one fixed focal exposure per participant\n")
  invisible(x)
}

#' Summarise prepared epikinetics model data
#'
#' @param object An `epikinetics_data` object.
#' @param ... Reserved for future methods.
#' @return A list of counts, ranges, mappings, censoring information, formula
#'   metadata, and design-matrix dimensions.
#' @export
summary.epikinetics_data <- function(object, ...) {
  censoring <- as.data.frame(table(
    factor(object$observations$censoring,
           levels = c("none", "left", "right"))
  ), stringsAsFactors = FALSE)
  names(censoring) <- c("censoring", "observations")
  structure(
    list(
      counts = c(
        observations = nrow(object$observations),
        participants = nrow(object$participants),
        biomarkers = length(object$mappings$biomarkers),
        covariates = ncol(object$model_matrix)
      ),
      ranges = data.frame(
        quantity = c("time_since_exposure", "response", "model_response"),
        minimum = c(
          min(object$observations$time_since_exposure),
          min(object$observations$value),
          min(object$observations$value_model)
        ),
        maximum = c(
          max(object$observations$time_since_exposure),
          max(object$observations$value),
          max(object$observations$value_model)
        ),
        stringsAsFactors = FALSE
      ),
      censoring = censoring,
      participant_observations = summary(object$participants$observation_count),
      participant_mapping = object$mappings$participant,
      biomarker_mapping = object$mappings$biomarker,
      covariates = object$mappings$covariates,
      design_columns = object$mappings$design_columns,
      variables = object$mappings$variables,
      covariate_parameters = object$mappings$covariate_parameters,
      participant_parameters = object$mappings$participant_parameters,
      biomarker_order = object$mappings$biomarkers,
      biomarker_order_source = object$specification$biomarker_order_source,
      factor_levels = object$mappings$factor_levels,
      reference_levels = object$mappings$reference_levels,
      formula = object$specification$formula,
      transformation = object$specification$transformation,
      model_matrix_dimensions = dim(object$model_matrix)
    ),
    class = "summary.epikinetics_data"
  )
}

#' @export
print.summary.epikinetics_data <- function(x, ...) {
  cat("Prepared epikinetics model-data summary\n")
  print(x$counts)
  cat("\nRanges\n")
  print(x$ranges, row.names = FALSE)
  cat("\nCensoring\n")
  print(x$censoring, row.names = FALSE)
  cat("\nParticipant observation counts\n")
  print(x$participant_observations)
  cat("\nFormula: ", paste(deparse(x$formula), collapse = " "), "\n", sep = "")
  cat("Transformation: ", x$transformation, "\n", sep = "")
  if (length(x$covariates)) {
    cat("Model-matrix columns: ", paste(x$covariates, collapse = ", "),
        "\n", sep = "")
    cat("Formula affects: ", paste(x$covariate_parameters, collapse = ", "),
        "\n", sep = "")
  } else {
    cat("Model-matrix columns: none\n")
  }
  cat("Participant random effects: ",
      if (length(x$participant_parameters)) {
        paste(x$participant_parameters, collapse = ", ")
      } else {
        "none"
      }, "\n", sep = "")
  cat("Biomarker order (", x$biomarker_order_source, "): ",
      paste(x$biomarker_order, collapse = ", "), "\n", sep = "")
  if (length(x$reference_levels)) {
    cat("Factor reference levels: ", paste(
      paste(names(x$reference_levels), x$reference_levels, sep = "="),
      collapse = ", "
    ), "\n", sep = "")
  }
  if (nrow(x$design_columns)) {
    cat("\nDesign-column mapping\n")
    print(x$design_columns, row.names = FALSE)
  }
  invisible(x)
}

#' Extract prepared model data
#'
#' @param x An `epikinetics_data` or `epikinetics_fit` object.
#' @return The labelled, model-ready observation data frame. Use [stan_data()]
#'   for the exact list passed to CmdStan.
#' @export
epikinetics_data <- function(x) {
  prepared <- if (inherits(x, "epikinetics_fit")) x$prepared else x
  if (!inherits(prepared, "epikinetics_data")) {
    stop("'x' must be an epikinetics_data or epikinetics_fit object.",
         call. = FALSE)
  }
  prepared$data
}

#' Access the exact Stan data list
#'
#' Returns the complete, sampling-ready list. For prepared data its default
#' `grainsize` may be adjusted by [fit_epikinetics()] for the requested thread
#' count. For a fit, the result includes the final grainsize used by that run.
#'
#' @param x An `epikinetics_data` or `epikinetics_fit` object.
#' @return A named list suitable for a CmdStanR model's `$sample(data = ...)`
#'   argument.
#' @export
stan_data <- function(x) {
  prepared <- if (inherits(x, "epikinetics_fit")) x$prepared else x
  if (!inherits(prepared, "epikinetics_data")) {
    stop("'x' must be an epikinetics_data or epikinetics_fit object.",
         call. = FALSE)
  }
  validate_epikinetics_stan_data(prepared$stan_data)
  prepared$stan_data
}

#' @export
model.matrix.epikinetics_data <- function(object, ...) {
  object$model_matrix
}

#' @export
model.matrix.epikinetics_fit <- function(object, ...) {
  object$prepared$model_matrix
}

validate_prepared_epikinetics_data <- function(x) {
  if (!inherits(x, "epikinetics_data")) {
    stop(
      "'model_data' must be created by prepare_epikinetics_data().",
      call. = FALSE
    )
  }
  required <- c(
    "input_data", "observations", "participants", "model_frame", "model_matrix",
    "stan_data", "priors", "mappings", "specification"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Prepared model data are missing component(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  validate_epikinetics_stan_data(x$stan_data)
  expected_dimensions <- c(
    x$stan_data$N_participants,
    x$stan_data$N_covariates
  )
  if (!is.matrix(x$model_matrix) ||
      !identical(dim(x$model_matrix), expected_dimensions) ||
      !isTRUE(all.equal(unname(x$model_matrix), x$stan_data$X,
                        check.attributes = FALSE))) {
    stop("The inspectable model_matrix and Stan X matrix are inconsistent.",
         call. = FALSE)
  }
  if (nrow(x$observations) != x$stan_data$N_observations ||
      nrow(x$participants) != x$stan_data$N_participants) {
    stop("Prepared observation or participant counts are inconsistent with ",
         "the Stan data.", call. = FALSE)
  }
  invisible(x)
}
