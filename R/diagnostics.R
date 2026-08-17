#' Summarise sampling diagnostics
#'
#' @param x An `epikinetics_fit` object.
#' @param run_cmdstan If `TRUE`, also run CmdStan's text diagnostic utility.
#' @param quiet Suppress the compact console report.
#'
#' @return Invisibly, a list containing sampler diagnostics and summaries for
#'   the principal population parameters.
#' @export
diagnose_epikinetics <- function(x, run_cmdstan = FALSE, quiet = FALSE) {
  if (!inherits(x, "epikinetics_fit") || !inherits(x$fit, "CmdStanFit")) {
    stop("Diagnostics require an epikinetics fit backed by CmdStanR.",
         call. = FALSE)
  }
  sampler <- x$fit$diagnostic_summary(quiet = TRUE)
  energy <- tryCatch(
    x$fit$sampler_diagnostics(format = "draws_array")[, , "energy__",
                                                         drop = FALSE],
    error = function(error) NULL
  )
  variables <- c(
    unname(.population_prefixes),
    "population_waning_change_time",
    "observation_sd"
  )
  parameters <- x$fit$summary(variables = variables)
  finite_or_na <- function(x, fun) {
    values <- x[is.finite(x)]
    if (length(values)) fun(values) else NA_real_
  }
  max_rhat <- finite_or_na(parameters$rhat, max)
  min_bulk_ess <- finite_or_na(parameters$ess_bulk, min)
  min_tail_ess <- finite_or_na(parameters$ess_tail, min)
  chain_diagnostics <- epikinetics_chain_diagnostics(sampler, energy)
  result <- list(
    sampler = sampler,
    chains = chain_diagnostics,
    parameters = parameters,
    overview = data.frame(
      divergences = sum(sampler$num_divergent),
      max_treedepth_hits = sum(sampler$num_max_treedepth),
      chains_with_nonfinite_ebfmi = sum(!is.finite(sampler$ebfmi)),
      chains_with_low_ebfmi = sum(
        is.finite(sampler$ebfmi) & sampler$ebfmi < 0.3
      ),
      max_rhat = max_rhat,
      min_bulk_ess = min_bulk_ess,
      min_tail_ess = min_tail_ess
    )
  )
  if (!quiet) {
    cat("epikinetics sampling diagnostics\n")
    print(result$overview, row.names = FALSE)
    problems <- chain_diagnostics$status != "ok"
    if (any(problems)) {
      cat("\nChain-level problems\n")
      print(chain_diagnostics[problems, ], row.names = FALSE)
    }
  }
  if (isTRUE(run_cmdstan)) {
    result$cmdstan <- x$fit$cmdstan_diagnose()
  }
  invisible(result)
}

epikinetics_chain_diagnostics <- function(sampler, energy = NULL) {
  n_chains <- length(sampler$ebfmi)
  out <- data.frame(
    chain = seq_len(n_chains),
    divergences = as.integer(sampler$num_divergent),
    max_treedepth_hits = as.integer(sampler$num_max_treedepth),
    ebfmi = as.numeric(sampler$ebfmi),
    finite_energy_draws = NA_integer_,
    unique_energy_values = NA_integer_,
    energy_variance = NA_real_,
    status = rep("ok", n_chains),
    stringsAsFactors = FALSE
  )

  if (!is.null(energy)) {
    energy_matrix <- matrix(
      as.numeric(energy),
      nrow = dim(energy)[1L],
      ncol = dim(energy)[2L]
    )
    for (chain in seq_len(n_chains)) {
      values <- energy_matrix[, chain]
      values <- values[is.finite(values)]
      out$finite_energy_draws[chain] <- length(values)
      out$unique_energy_values[chain] <- length(unique(values))
      out$energy_variance[chain] <- if (length(values) > 1L) {
        stats::var(values)
      } else {
        NA_real_
      }
    }
  }

  nonfinite <- !is.finite(out$ebfmi)
  no_energy <- nonfinite & !is.na(out$finite_energy_draws) &
    out$finite_energy_draws < 2L
  constant_energy <- nonfinite & !is.na(out$energy_variance) &
    out$energy_variance == 0
  out$status[nonfinite] <- "non-finite E-BFMI"
  out$status[no_energy] <- "non-finite E-BFMI: fewer than two finite energies"
  out$status[constant_energy] <- "non-finite E-BFMI: constant energy"
  out$status[is.finite(out$ebfmi) & out$ebfmi < 0.3] <- "low E-BFMI"
  out$status[out$max_treedepth_hits > 0 & out$status == "ok"] <-
    "maximum treedepth reached"
  out$status[out$divergences > 0 & out$status == "ok"] <- "divergences"
  out
}

warn_if_pathological_sampling <- function(x) {
  if (!inherits(x$fit, "CmdStanFit")) return(invisible(FALSE))
  sampler <- tryCatch(
    x$fit$diagnostic_summary(quiet = TRUE),
    error = function(error) NULL
  )
  if (is.null(sampler)) return(invisible(FALSE))

  invalid <- which(!is.finite(sampler$ebfmi))
  if (length(invalid)) {
    warning(
      "Prediction includes chain(s) ", paste(invalid, collapse = ", "),
      " with non-finite E-BFMI. A chain may be frozen, so posterior ",
      "trajectories and intervals can be misleading. Run ",
      "diagnose_epikinetics(fit) and resolve sampling problems before ",
      "interpreting predictions.",
      call. = FALSE
    )
    return(invisible(TRUE))
  }
  if (sum(sampler$num_divergent) > 0L ||
      sum(sampler$num_max_treedepth) > 0L) {
    warning(
      "Prediction uses a fit with divergent transitions or maximum-",
      "treedepth hits. Run diagnose_epikinetics(fit) before interpreting ",
      "posterior trajectories.",
      call. = FALSE
    )
    return(invisible(TRUE))
  }
  invisible(FALSE)
}
