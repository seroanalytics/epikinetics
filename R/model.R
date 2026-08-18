epikinetics_stan_file <- function() {
  installed <- system.file("stan", "epikinetics.stan", package = "epikinetics")
  if (nzchar(installed)) {
    return(installed)
  }

  source_file <- file.path("inst", "stan", "epikinetics.stan")
  if (file.exists(source_file)) {
    return(normalizePath(source_file, mustWork = TRUE))
  }

  stop("Could not locate the epikinetics Stan source file.", call. = FALSE)
}

check_cmdstan <- function() {
  version <- tryCatch(
    cmdstanr::cmdstan_version(error_on_NA = FALSE),
    error = function(error) NULL
  )
  if (is.null(version) || length(version) == 0L || anyNA(version)) {
    stop(
      "CmdStan is required to fit epikinetics models but was not found. ",
      "Install and verify it explicitly with:\n\n",
      "  cmdstanr::check_cmdstan_toolchain(fix = TRUE)\n",
      "  cmdstanr::install_cmdstan()\n",
      "  cmdstanr::cmdstan_version()",
      call. = FALSE
    )
  }
  if (utils::compareVersion(as.character(version), "2.26.0") < 0L) {
    stop(
      "epikinetics requires CmdStan 2.26 or newer; found ",
      as.character(version), ". Update it explicitly with:\n\n",
      "  cmdstanr::install_cmdstan()",
      call. = FALSE
    )
  }
  invisible(version)
}

#' Compile the threaded epikinetics Stan model
#'
#' Compilation is deliberately explicit and cached in R's per-user cache
#' directory. Package installation and loading never install CmdStan or compile
#' C++. [fit_epikinetics()] calls this function automatically when needed.
#'
#' @param force_recompile Recompile even when the current source has a cached
#'   executable.
#' @param quiet Passed to [cmdstanr::cmdstan_model()].
#' @param cache_dir Optional cache directory. The default is controlled by the
#'   `epikinetics.cache_dir` option and otherwise uses [tools::R_user_dir()].
#'
#' @return A `CmdStanModel` compiled with Stan threading enabled.
#' @export
compile_epikinetics_model <- function(
    force_recompile = FALSE,
    quiet = TRUE,
    cache_dir = getOption(
      "epikinetics.cache_dir",
      tools::R_user_dir("epikinetics", which = "cache")
    )) {
  cmdstan_version <- check_cmdstan()
  if (!is.logical(force_recompile) || length(force_recompile) != 1L ||
      is.na(force_recompile)) {
    stop("'force_recompile' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L || !nzchar(cache_dir)) {
    stop("'cache_dir' must be one non-empty path.", call. = FALSE)
  }
  if (!dir.exists(cache_dir) &&
      !dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the epikinetics model cache at '", cache_dir, "'.",
         call. = FALSE)
  }

  source <- epikinetics_stan_file()
  hash <- unname(tools::md5sum(source))
  version_tag <- gsub("[^0-9A-Za-z.-]", "-", as.character(cmdstan_version))
  cached_source <- file.path(
    cache_dir,
    paste0("epikinetics-", hash, "-cmdstan-", version_tag, ".stan")
  )
  if (!file.exists(cached_source) && !file.copy(source, cached_source)) {
    stop("Could not copy the Stan source into the model cache.", call. = FALSE)
  }

  cmdstanr::cmdstan_model(
    stan_file = cached_source,
    compile = TRUE,
    force_recompile = force_recompile,
    cpp_options = list(stan_threads = TRUE),
    stanc_options = list("O1"),
    quiet = quiet
  )
}
