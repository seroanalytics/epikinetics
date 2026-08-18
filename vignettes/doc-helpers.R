# Small, shared helpers for figures used across the documentation. These are
# deliberately vignette-only: they keep the package API focused while making
# repeated illustrations reproducible and visually consistent.

doc_biomarker_order <- c("Ancestral", "Alpha", "Delta")

doc_biomarker_palette <- c(
  Ancestral = "#0072B2",
  Alpha = "#D55E00",
  Delta = "#009E73"
)

doc_delta_data <- function() {
  utils::read.csv(
    system.file("extdata", "delta.csv", package = "epikinetics"),
    stringsAsFactors = FALSE
  )
}

doc_prepare_delta <- function(data = doc_delta_data(), lower_limit = 5) {
  prepare_epikinetics_data(
    data,
    formula = ~ infection_history,
    biomarker_order = doc_biomarker_order,
    lower_limit = lower_limit,
    upper_limit = 2560
  )
}

doc_participant_summary <- function(data) {
  visits <- unique(data[c("pid", "infection_history", "day", "last_exp_day")])
  visits$time <- as.numeric(as.Date(visits$day) - as.Date(visits$last_exp_day))

  count <- stats::aggregate(
    time ~ pid + infection_history, visits, length
  )
  span <- stats::aggregate(
    time ~ pid + infection_history, visits, function(x) diff(range(x))
  )
  last <- stats::aggregate(
    time ~ pid + infection_history, visits, max
  )
  out <- Reduce(
    function(x, y) merge(x, y, by = c("pid", "infection_history")),
    list(count, span, last)
  )
  names(out) <- c(
    "pid", "infection_history", "n_visits", "follow_up_span", "last_visit"
  )
  out
}

doc_representative_participants <- function(data, n = 9L) {
  summary <- doc_participant_summary(data)
  histories <- unique(data$infection_history)
  allocation <- round(n * table(factor(summary$infection_history,
                                       levels = histories)) / nrow(summary))
  allocation[which.max(allocation)] <- allocation[which.max(allocation)] +
    (n - sum(allocation))

  selected <- lapply(seq_along(histories), function(i) {
    stratum <- summary[summary$infection_history == histories[i], , drop = FALSE]
    number <- allocation[i]
    if (!number) return(stratum[FALSE, , drop = FALSE])

    information <- as.numeric(scale(log1p(stratum$n_visits))) +
      as.numeric(scale(log1p(stratum$follow_up_span)))
    if (all(!is.finite(information))) information <- seq_len(nrow(stratum))
    information[!is.finite(information)] <- 0
    stratum <- stratum[order(information, as.character(stratum$pid)), , drop = FALSE]
    index <- unique(pmax(
      1L,
      pmin(nrow(stratum), round(seq(0.12, 0.88, length.out = number) * nrow(stratum)))
    ))
    stratum[index, , drop = FALSE]
  })

  out <- do.call(rbind, selected)
  rownames(out) <- NULL
  out
}

doc_raw_data_plot <- function(
    data = doc_delta_data(),
    participants = NULL,
    aligned = FALSE,
    ncol = 3L) {
  if (is.null(participants)) {
    participants <- doc_representative_participants(data, 9L)
  }
  keep <- as.character(data$pid) %in% as.character(participants$pid)
  plot_data <- data[keep, , drop = FALSE]
  plot_data$time_since_exposure <- as.numeric(
    as.Date(plot_data$day) - as.Date(plot_data$last_exp_day)
  )
  plot_data$sample_date <- as.Date(plot_data$day)
  plot_data$exposure_date <- as.Date(plot_data$last_exp_day)
  plot_data$biomarker <- factor(
    plot_data$titre_type,
    levels = doc_biomarker_order
  )
  history_label <- ifelse(
    plot_data$infection_history == "Infection naive",
    "infection-naive",
    "previously infected"
  )
  plot_data$panel <- paste0("Participant ", plot_data$pid, "\n", history_label)
  panel_levels <- paste0(
    "Participant ", participants$pid, "\n",
    ifelse(
      participants$infection_history == "Infection naive",
      "infection-naive",
      "previously infected"
    )
  )
  plot_data$panel <- factor(plot_data$panel, levels = panel_levels)
  plot_data$plot_time <- if (isTRUE(aligned)) {
    plot_data$time_since_exposure
  } else {
    plot_data$sample_date
  }
  exposure_lines <- unique(plot_data[c("panel", "exposure_date")])
  exposure_lines$plot_time <- if (isTRUE(aligned)) {
    0
  } else {
    exposure_lines$exposure_date
  }
  line_group <- interaction(plot_data$pid, plot_data$biomarker, drop = TRUE)
  line_data <- plot_data[
    stats::ave(seq_len(nrow(plot_data)), line_group, FUN = length) > 1L,
    ,
    drop = FALSE
  ]

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = plot_time,
      y = value,
      colour = biomarker,
      group = interaction(pid, biomarker)
    )
  ) +
    ggplot2::geom_vline(
      data = exposure_lines,
      ggplot2::aes(xintercept = plot_time),
      inherit.aes = FALSE,
      colour = "grey45",
      linetype = "dashed",
      linewidth = 0.38
    ) +
    ggplot2::geom_line(
      data = line_data,
      linewidth = 0.48,
      alpha = 0.58
    ) +
    ggplot2::geom_point(size = 1.45, alpha = 0.82) +
    ggplot2::facet_wrap(
      ggplot2::vars(panel),
      ncol = ncol,
      scales = if (isTRUE(aligned)) "free_x" else "fixed"
    ) +
    ggplot2::scale_colour_manual(
      values = doc_biomarker_palette,
      limits = doc_biomarker_order,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      trans = "log2",
      breaks = c(5, 20, 80, 320, 1280),
      labels = c("5", "20", "80", "320", "1,280")
    ) +
    ggplot2::labs(
      x = if (isTRUE(aligned)) {
        "Days since focal exposure"
      } else {
        "Sample collection date"
      },
      y = "Neutralising titre",
      colour = "Biomarker"
    ) +
    ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey91", linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 8.5),
      panel.spacing = grid::unit(0.8, "lines"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(6, 8, 6, 6)
    ) +
    if (isTRUE(aligned)) {
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.03, 0.04))
      )
    } else {
      ggplot2::scale_x_date(
        date_breaks = "2 months",
        date_labels = "%b\n%Y",
        expand = ggplot2::expansion(mult = c(0.03, 0.04))
      )
    }
}

doc_censoring_plot <- function() {
  seed_existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_existed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (seed_existed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(20260818)
  n_draws <- 800L
  parameter_draws <- data.frame(
    baseline = stats::rnorm(n_draws, mean = 5, sd = 0.10),
    time_to_peak = exp(stats::rnorm(n_draws, log(24), 0.035)),
    switch_duration = exp(stats::rnorm(n_draws, log(48), 0.08)),
    boost_rate = exp(stats::rnorm(n_draws, log(0.26), 0.04)),
    early_waning_rate = exp(stats::rnorm(n_draws, log(0.035), 0.10)),
    late_waning_rate = exp(stats::rnorm(n_draws, log(0.009), 0.14))
  )
  parameter_draws$waning_change_time <-
    parameter_draws$time_to_peak + parameter_draws$switch_duration

  time <- seq(-28, 180, by = 1)
  model_time <- pmax(time, 0)
  kinetic_function <- utils::getFromNamespace("kinetics_mean", "epikinetics")
  latent_draws <- vapply(
    seq_len(n_draws),
    function(draw) {
      latent_model <- kinetic_function(
        model_time,
        rep(parameter_draws$baseline[draw], length(time)),
        rep(parameter_draws$time_to_peak[draw], length(time)),
        rep(parameter_draws$waning_change_time[draw], length(time)),
        rep(parameter_draws$boost_rate[draw], length(time)),
        rep(parameter_draws$early_waning_rate[draw], length(time)),
        rep(parameter_draws$late_waning_rate[draw], length(time))
      )
      latent_model[time < 0] <- parameter_draws$baseline[draw]
      2^latent_model
    },
    numeric(length(time))
  )

  curve <- data.frame(
    time = time,
    median = apply(latent_draws, 1L, stats::median),
    lower = apply(latent_draws, 1L, stats::quantile, probs = 0.025),
    upper = apply(latent_draws, 1L, stats::quantile, probs = 0.975)
  )
  observation_time <- c(-14, 22, 30, 95, 165)
  observation_median <- stats::approx(
    curve$time, curve$median, observation_time
  )$y
  observed <- data.frame(
    time = observation_time,
    status = factor(
      c("none", "right", "right", "none", "none"),
      levels = c("none", "right")
    ),
    recorded = c(
      observation_median[1] * 0.94,
      1024,
      1024,
      observation_median[4] * 1.05,
      observation_median[5] * 0.92
    )
  )

  ggplot2::ggplot(curve, ggplot2::aes(time, median)) +
    ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = 1024, ymax = 4100,
      fill = "#D55E00", alpha = 0.055
    ) +
    ggplot2::geom_hline(
      yintercept = 1024, linetype = "dashed", colour = "grey45",
      linewidth = 0.45
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed", colour = "#0072B2",
      linewidth = 0.5
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "#0072B2", alpha = 0.17, colour = NA
    ) +
    ggplot2::geom_line(colour = "grey22", linewidth = 0.9) +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(y = recorded, shape = status, fill = status),
      size = 2.6,
      stroke = 0.55,
      colour = "grey20"
    ) +
    ggplot2::annotate(
      "text", x = 176, y = 960, label = "upper assay limit = 1,024",
      hjust = 1, vjust = 1, colour = "grey35", size = 3.2
    ) +
    ggplot2::annotate(
      "text", x = 3, y = 145, label = "exposure", hjust = 0,
      vjust = 0, colour = "#0072B2", size = 3.2
    ) +
    ggplot2::scale_shape_manual(
      values = c(none = 21, right = 24),
      labels = c(none = "Observed", right = "Upper-censored")
    ) +
    ggplot2::scale_fill_manual(
      values = c(none = "white", right = "#D55E00"),
      labels = c(none = "Observed", right = "Upper-censored")
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(-25, 0, 25, 75, 125, 175),
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1024, 2048, 3072, 4096),
      labels = c("0", "1,024", "2,048", "3,072", "4,096"),
      limits = c(0, 4100),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::labs(
      x = "Time since exposure",
      y = "Neutralising titre",
      shape = "Measurement",
      fill = "Measurement"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}

doc_figure <- function(filename) {
  knitr::include_graphics(file.path("figures", filename))
}
