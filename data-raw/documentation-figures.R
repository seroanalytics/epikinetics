# Rebuild the static figures used by the README and short vignettes.
#
# The raw-data figure is inexpensive. Fit-derived figures are regenerated only
# when EPIKINETICS_REBUILD_FIT_FIGURES=true because they require CmdStan and a
# substantive (though deliberately modest) four-chain fit. Routine package and
# pkgdown builds use the committed outputs and never refit the model.

pkgload::load_all(".", quiet = TRUE)
source("vignettes/doc-helpers.R")

save_documentation_plot <- function(plot, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path("man", "figures", filename),
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = 180,
    background = "white"
  )
}

sync_documentation_assets <- function() {
  assets <- Sys.glob(file.path("man", "figures", "documentation-*"))
  file.copy(
    assets,
    file.path("vignettes", "figures", basename(assets)),
    overwrite = TRUE
  )
  invisible(assets)
}

delta_data <- doc_delta_data()
representatives <- doc_representative_participants(delta_data, 9L)
save_documentation_plot(
  doc_raw_data_plot(delta_data, representatives),
  "documentation-longitudinal-data.png",
  width = 8.4,
  height = 6.2
)
sync_documentation_assets()

reuse_csv_dir <- Sys.getenv("EPIKINETICS_DOC_CSV_DIR", "")
run_fit <- nzchar(reuse_csv_dir) || identical(
  tolower(Sys.getenv("EPIKINETICS_REBUILD_FIT_FIGURES", "false")),
  "true"
)
if (!run_fit) {
  message(
    "Raw-data figure rebuilt. Set EPIKINETICS_REBUILD_FIT_FIGURES=true ",
    "to refit and rebuild posterior figures."
  )
  quit(save = "no", status = 0L)
}

prepared <- doc_prepare_delta(delta_data, lower_limit = 5)
if (nzchar(reuse_csv_dir)) {
  csv_files <- list.files(
    reuse_csv_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  if (length(csv_files) != 4L) {
    stop("EPIKINETICS_DOC_CSV_DIR must contain the four documentation chains.")
  }
  cmdstan_result <- cmdstanr::as_cmdstan_fit(csv_files)
  fit <- structure(
    list(
      fit = cmdstan_result,
      prepared = prepared,
      call = quote(documentation_fit_reused_from_csv()),
      computation = list(
        chains = 4L,
        parallel_chains = 4L,
        threads_per_chain = 2L,
        grainsize = prepared$stan_data$grainsize,
        adapt_delta = 0.95,
        max_treedepth = 12L,
        sampling_state = "complete",
        return_codes = rep(0L, 4L)
      ),
      model = list(
        package_version = "development",
        cmdstan_version = as.character(cmdstanr::cmdstan_version())
      )
    ),
    class = "epikinetics_fit"
  )
} else {
  documentation_output_dir <- Sys.getenv("EPIKINETICS_DOC_OUTPUT_DIR", "")
  if (!nzchar(documentation_output_dir)) {
    documentation_output_dir <- file.path(
      tempdir(), "epikinetics-documentation-fit"
    )
  }
  dir.create(documentation_output_dir, recursive = TRUE, showWarnings = FALSE)
  fit <- fit_epikinetics(
    prepared,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 2,
    iter_warmup = 500,
    iter_sampling = 500,
    adapt_delta = 0.95,
    seed = 2026,
    output_dir = documentation_output_dir
  )
}

diagnostics <- diagnose_epikinetics(fit, quiet = TRUE)
overview <- diagnostics$overview[1, , drop = FALSE]
if (!identical(fit$computation$sampling_state, "complete") ||
    overview$divergences > 0L ||
    overview$max_treedepth_hits > 0L ||
    overview$chains_with_nonfinite_ebfmi > 0L ||
    overview$chains_with_low_ebfmi > 0L ||
    !is.finite(overview$max_rhat) ||
    overview$max_rhat >= 1.01) {
  stop(
    "The documentation fit did not complete with healthy diagnostics; ",
    "figures were not updated."
  )
}

population <- predict(
  fit,
  type = "population",
  times = 0:150,
  ndraws = 1000
)
population_plot <- plot(population) +
  ggplot2::labs(
    title = "Conditional population kinetics",
    x = "Days since focal exposure",
    y = "Neutralising titre"
  )
save_documentation_plot(
  population_plot,
  "documentation-population-kinetics.png",
  width = 8.4,
  height = 4.8
)

# Use the most repeatedly observed member of the deterministic documentation
# subset. Selection depends only on the raw sampling design, not fitted values.
detail_id <- as.character(
  representatives$pid[which.max(representatives$n_visits)]
)
individual <- predict(
  fit,
  type = "individual",
  participants = detail_id,
  times = 0:220,
  ndraws = 1000
)
individual_plot <- plot_individual(individual, participant = detail_id) +
  ggplot2::labs(
    title = paste("Participant", detail_id),
    x = "Days since focal exposure",
    y = "Neutralising titre"
  )
save_documentation_plot(
  individual_plot,
  "documentation-individual-kinetics.png",
  width = 8.4,
  height = 4.8
)

trace_variables <- c(
  "population_time_to_peak[1]" = "Ancestral time to peak",
  "population_late_waning_rate[1]" = "Ancestral late waning rate"
)
trace_draws <- posterior::as_draws_df(
  cmdstan_fit(fit)$draws(variables = names(trace_variables))
)
trace_data <- do.call(rbind, lapply(names(trace_variables), function(variable) {
  data.frame(
    iteration = trace_draws$.iteration,
    chain = factor(trace_draws$.chain),
    parameter = trace_variables[[variable]],
    value = trace_draws[[variable]]
  )
}))
density_data <- do.call(rbind, lapply(
  split(trace_data, list(trace_data$parameter, trace_data$chain), drop = TRUE),
  function(group) {
    estimate <- stats::density(group$value, n = 160)
    data.frame(
      chain = group$chain[1],
      parameter = group$parameter[1],
      x = estimate$x,
      y = estimate$y,
      diagnostic = "Density"
    )
  }
))
trace_panels <- transform(
  trace_data,
  x = iteration,
  y = value,
  diagnostic = "Trace"
)[c("chain", "parameter", "x", "y", "diagnostic")]
diagnostic_data <- rbind(trace_panels, density_data)
diagnostic_data$panel <- factor(
  paste(diagnostic_data$parameter, diagnostic_data$diagnostic,
        paste("chain", diagnostic_data$chain), sep = "\n"),
  levels = unlist(lapply(trace_variables, function(parameter) {
    c(
      paste(parameter, "Trace", paste("chain", 1:4), sep = "\n"),
      paste(parameter, "Density", paste("chain", 1:4), sep = "\n")
    )
  }))
)
trace_plot <- ggplot2::ggplot(
  diagnostic_data,
  ggplot2::aes(x, y, colour = chain, group = chain)
) +
  ggplot2::geom_line(linewidth = 0.32, alpha = 0.78) +
  ggplot2::facet_wrap(ggplot2::vars(panel), scales = "free", ncol = 4) +
  ggplot2::scale_colour_manual(
    values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
    guide = "none"
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "grey91", linewidth = 0.25),
    strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
    strip.text = ggplot2::element_text(face = "bold", size = 7.8),
    axis.text = ggplot2::element_text(size = 7.4),
    panel.spacing = grid::unit(0.75, "lines")
  )
save_documentation_plot(
  trace_plot,
  "documentation-diagnostics.png",
  width = 8.4,
  height = 7.2
)
sync_documentation_assets()

message("Documentation figures rebuilt from a completed four-chain fit.")
