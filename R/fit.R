#' Fit a hierarchical biomarker kinetics model
#'
#' Fits the epikinetics piecewise-linear model with CmdStanR and returns a
#' self-contained S3 fit object. The likelihood supports uncensored, left-
#' censored, and right-censored observations. Parallel chains and Stan threads
#' within each chain can be used together. Data validation and transformation
#' are deliberately performed beforehand by [prepare_epikinetics_data()].
#'
#' @param model_data An object returned by [prepare_epikinetics_data()].
#' @param chains Number of Markov chains.
#' @param parallel_chains Number of chains to run concurrently.
#' @param threads_per_chain Number of Stan threads used by each chain. The model
#'   is always compiled with threading enabled.
#' @param grainsize Number of participants handled by each `reduce_sum` task.
#'   By default it is chosen from the participant and thread counts.
#' @param adapt_delta Target acceptance probability. The conservative default
#'   is appropriate for this nonlinear hierarchy and may be overridden after
#'   inspecting diagnostics.
#' @param max_treedepth Maximum NUTS tree depth.
#' @param ... Additional arguments passed to the CmdStanR model's `$sample()`
#'   method, such as `iter_warmup`, `iter_sampling`, and `seed`.
#'
#' @return An object of class `epikinetics_fit`.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- utils::read.csv(
#'   system.file("extdata", "delta.csv", package = "epikinetics")
#' )
#' model_data <- prepare_epikinetics_data(
#'   dat,
#'   formula = ~ infection_history,
#'   lower_limit = 5,
#'   upper_limit = 2560
#' )
#' model_data
#' model.matrix(model_data)
#' stan_data(model_data)
#'
#' fit <- fit_epikinetics(
#'   model_data,
#'   chains = 4,
#'   parallel_chains = 4,
#'   threads_per_chain = 2
#' )
#' }
fit_epikinetics <- function(
    model_data,
    chains = 4,
    parallel_chains = chains,
    threads_per_chain = 1,
    grainsize = NULL,
    adapt_delta = 0.9,
    max_treedepth = 12,
    ...) {
  call <- match.call()
  validate_prepared_epikinetics_data(model_data)
  prepared <- model_data

  chains <- validate_count(chains, "chains")
  parallel_chains <- validate_count(parallel_chains, "parallel_chains")
  threads_per_chain <- validate_count(threads_per_chain, "threads_per_chain")
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L ||
      !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    stop("'adapt_delta' must be one number strictly between 0 and 1.",
         call. = FALSE)
  }
  max_treedepth <- validate_count(max_treedepth, "max_treedepth")
  if (parallel_chains > chains) {
    stop("'parallel_chains' cannot exceed 'chains'.", call. = FALSE)
  }

  n_participants <- prepared$stan_data$N_participants
  if (is.null(grainsize)) {
    grainsize <- if (threads_per_chain == 1L) {
      n_participants
    } else {
      max(1L, as.integer(ceiling(n_participants / (4 * threads_per_chain))))
    }
  } else {
    grainsize <- validate_count(grainsize, "grainsize")
  }
  prepared$stan_data$grainsize <- grainsize

  model <- compile_epikinetics_model()
  cmdstan_result <- model$sample(
    data = prepared$stan_data,
    chains = chains,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    ...
  )
  return_codes <- tryCatch(
    cmdstan_result$return_codes(),
    error = function(error) integer()
  )
  sampling_state <- if (!length(return_codes)) {
    "unknown"
  } else if (all(return_codes == 0L)) {
    "complete"
  } else if (any(return_codes == 0L)) {
    "partial"
  } else {
    "failed"
  }

  model_source <- epikinetics_stan_file()
  result <- structure(
    list(
      fit = cmdstan_result,
      prepared = prepared,
      call = call,
      computation = list(
        chains = chains,
        parallel_chains = parallel_chains,
        threads_per_chain = threads_per_chain,
        grainsize = grainsize,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth,
        sampling_state = sampling_state,
        return_codes = return_codes
      ),
      model = list(
        source_hash = unname(tools::md5sum(model_source)),
        package_version = tryCatch(
          as.character(utils::packageVersion("epikinetics")),
          error = function(error) "development"
        ),
        cmdstan_version = as.character(
          cmdstanr::cmdstan_version(error_on_NA = FALSE)
        )
      )
    ),
    class = c(
      if (sampling_state == "failed") "epikinetics_failed_fit",
      "epikinetics_fit"
    )
  )
  if (sampling_state == "failed") {
    warning(
      "All CmdStan chains failed. The returned epikinetics_fit preserves the ",
      "CmdStanMCMC run; inspect cmdstan_fit(fit)$output() for the complete ",
      "chain output.",
      call. = FALSE
    )
  } else if (sampling_state == "partial") {
    warning(
      "Some CmdStan chains failed. Inspect ",
      "cmdstan_fit(fit)$return_codes() and cmdstan_fit(fit)$output().",
      call. = FALSE
    )
  }
  result
}

validate_count <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != as.integer(x)) {
    stop("'", argument, "' must be one positive integer.", call. = FALSE)
  }
  as.integer(x)
}

#' Access the underlying CmdStanR fit
#'
#' @param x An `epikinetics_fit` object.
#' @return The underlying `CmdStanMCMC` object.
#' @export
cmdstan_fit <- function(x) {
  if (!inherits(x, "epikinetics_fit")) {
    stop("'x' must be an epikinetics_fit object.", call. = FALSE)
  }
  x$fit
}

#' @export
print.epikinetics_fit <- function(x, ...) {
  cat("epikinetics model fit\n")
  cat("  Observations: ", nrow(x$prepared$data), "\n", sep = "")
  cat("  Participants: ", length(x$prepared$mappings$participants), "\n",
      sep = "")
  cat("  Biomarkers:   ",
      paste(x$prepared$mappings$biomarkers, collapse = ", "), "\n", sep = "")
  formula_variables <- all.vars(x$prepared$specification$formula)
  cat("  Covariates:   ",
      if (length(formula_variables)) {
        paste(formula_variables, collapse = ", ")
      } else {
        "none"
      }, "\n", sep = "")
  cat("  Computation:  ", x$computation$chains, " chain(s), ",
      x$computation$threads_per_chain, " thread(s) per chain\n", sep = "")
  if (!is.null(x$computation$sampling_state)) {
    cat("  Sampling:     ", x$computation$sampling_state, "\n", sep = "")
  }
  if (identical(x$computation$sampling_state, "failed")) {
    cat("  Debug with:   cmdstan_fit(fit)$output()\n")
  }
  invisible(x)
}

#' @export
summary.epikinetics_fit <- function(object, ...) {
  variables <- c(
    unname(.population_prefixes),
    "population_waning_change_time",
    "observation_sd"
  )
  if (inherits(object$fit, "CmdStanFit")) {
    return(object$fit$summary(variables = variables, ...))
  }
  posterior::summarise_draws(
    posterior_draws(object, variables = variables, format = "draws_array"),
    ...
  )
}
