#' Priors for the epikinetics model
#'
#' Construct and inspect the priors used by [fit_epikinetics()]. Population
#' priors are Normal distributions parameterised by `c(mean, sd)`. Positive
#' kinetic quantities use Normal priors truncated at zero. Their prior means
#' must be non-negative so that the inverse-CDF parameterisation remains
#' numerically stable.
#'
#' Baseline is expressed on the model's log2 scale. Time parameters are in the
#' same units as the input time columns (normally days). Rates are log2 units
#' per time unit. Participant standard deviations act additively for baseline
#' and on the log scale for positive time/rate parameters. Covariate priors use
#' the same convention.
#'
#' @param baseline,time_to_peak,waning_duration,boost_rate,early_waning_rate,late_waning_rate
#'   Numeric length-two vectors giving the population prior mean and standard
#'   deviation.
#' @param participant_sd Named positive vector giving half-Normal scales for
#'   participant-level standard deviations.
#' @param covariate_sd Named positive vector giving Normal standard deviations
#'   for regression coefficients.
#' @param observation_sd Positive half-Normal scale for observation error.
#'
#' @return An object of class `epikinetics_priors`.
#' @export
#'
#' @examples
#' priors <- epikinetics_priors(
#'   time_to_peak = c(mean = 12, sd = 4),
#'   early_waning_rate = c(mean = 0.025, sd = 0.01)
#' )
#' priors
epikinetics_priors <- function(
    baseline = c(mean = 6, sd = 2),
    time_to_peak = c(mean = 10, sd = 5),
    waning_duration = c(mean = 50, sd = 20),
    boost_rate = c(mean = 0.25, sd = 0.10),
    early_waning_rate = c(mean = 0.02, sd = 0.01),
    late_waning_rate = c(mean = 0.002, sd = 0.005),
    participant_sd = c(
      baseline = 0.75,
      time_to_peak = 0.35,
      waning_duration = 0.50,
      boost_rate = 0.35,
      early_waning_rate = 0.50,
      late_waning_rate = 0.75
    ),
    covariate_sd = c(
      baseline = 0.50,
      time_to_peak = 0.35,
      waning_duration = 0.50,
      boost_rate = 0.35,
      early_waning_rate = 0.50,
      late_waning_rate = 0.75
    ),
    observation_sd = 1) {
  population <- list(
    baseline = baseline,
    time_to_peak = time_to_peak,
    waning_duration = waning_duration,
    boost_rate = boost_rate,
    early_waning_rate = early_waning_rate,
    late_waning_rate = late_waning_rate
  )

  population <- lapply(names(population), function(parameter) {
    value <- population[[parameter]]
    if (!is.numeric(value) || length(value) != 2L || any(!is.finite(value))) {
      stop("'", parameter, "' must be a finite numeric c(mean, sd) vector.",
           call. = FALSE)
    }
    if (unname(value[2L]) <= 0) {
      stop("The prior SD for '", parameter, "' must be positive.",
           call. = FALSE)
    }
    if (parameter != "baseline" && unname(value[1L]) < 0) {
      stop("The prior mean for positive parameter '", parameter,
           "' must be non-negative.", call. = FALSE)
    }
    stats::setNames(as.numeric(value), c("mean", "sd"))
  }) |>
    stats::setNames(.epikinetics_parameters)

  participant_sd <- validate_named_prior_scale(
    participant_sd,
    "participant_sd"
  )
  covariate_sd <- validate_named_prior_scale(covariate_sd, "covariate_sd")

  if (!is.numeric(observation_sd) || length(observation_sd) != 1L ||
      !is.finite(observation_sd) || observation_sd <= 0) {
    stop("'observation_sd' must be one finite positive number.", call. = FALSE)
  }

  structure(
    list(
      population = population,
      participant_sd = participant_sd,
      covariate_sd = covariate_sd,
      observation_sd = as.numeric(observation_sd)
    ),
    class = "epikinetics_priors"
  )
}

validate_named_prior_scale <- function(x, argument) {
  if (!is.numeric(x) || is.null(names(x))) {
    stop("'", argument, "' must be a named numeric vector.", call. = FALSE)
  }
  missing <- setdiff(.epikinetics_parameters, names(x))
  unknown <- setdiff(names(x), .epikinetics_parameters)
  if (length(missing) || length(unknown)) {
    detail <- c(
      if (length(missing)) paste("missing:", paste(missing, collapse = ", ")),
      if (length(unknown)) paste("unknown:", paste(unknown, collapse = ", "))
    )
    stop("'", argument, "' has invalid names (", paste(detail, collapse = "; "),
         ").", call. = FALSE)
  }
  x <- x[.epikinetics_parameters]
  if (any(!is.finite(x)) || any(x <= 0)) {
    stop("All '", argument, "' values must be finite and positive.",
         call. = FALSE)
  }
  x
}

#' @export
print.epikinetics_priors <- function(x, ...) {
  population <- do.call(rbind, x$population)
  out <- data.frame(
    parameter = rownames(population),
    population_mean = population[, "mean"],
    population_sd = population[, "sd"],
    participant_sd_scale = unname(x$participant_sd[rownames(population)]),
    covariate_sd = unname(x$covariate_sd[rownames(population)]),
    row.names = NULL,
    check.names = FALSE
  )
  print(out, row.names = FALSE)
  cat("\nObservation SD half-Normal scale:", x$observation_sd, "\n")
  invisible(x)
}

priors_to_stan <- function(priors) {
  if (!inherits(priors, "epikinetics_priors")) {
    stop("'priors' must be created by epikinetics_priors().", call. = FALSE)
  }
  population <- do.call(rbind, priors$population)
  list(
    population_prior_mean = unname(population[, "mean"]),
    population_prior_sd = unname(population[, "sd"]),
    participant_sd_prior_scale = unname(
      priors$participant_sd[.epikinetics_parameters]
    ),
    covariate_prior_scale = unname(
      priors$covariate_sd[.epikinetics_parameters]
    ),
    observation_sd_prior_scale = priors$observation_sd
  )
}
