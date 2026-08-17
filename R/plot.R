epikinetics_palette <- function(levels) {
  n <- length(levels)
  if (n > 12L) {
    warning(
      "The plot contains ", n, " biomarkers; colour distinctions are unlikely ",
      "to remain reliable. Subset 'biomarkers' or replace the ggplot colour ",
      "and fill scales.",
      call. = FALSE
    )
  }
  okabe_ito <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#000000", "#F0E442"
  )
  colours <- if (n <= length(okabe_ito)) {
    okabe_ito[seq_len(n)]
  } else {
    grDevices::hcl.colors(n, palette = "Dark 3")
  }
  stats::setNames(colours, levels)
}

theme_epikinetics <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(
        colour = "grey35", margin = ggplot2::margin(b = 8)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey90", linewidth = 0.35
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey94", colour = NA
      ),
      strip.text = ggplot2::element_text(face = "bold", colour = "grey20"),
      panel.spacing = grid::unit(1, "lines"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(colour = "grey20"),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

epikinetics_axis_scales <- function(scale) {
  list(
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.01, 0.025))
    ),
    ggplot2::scale_y_continuous(
      trans = if (scale == "response") "log2" else "identity",
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    )
  )
}

add_epikinetics_scales <- function(plot, biomarker_levels, scale) {
  palette <- epikinetics_palette(biomarker_levels)
  plot +
    ggplot2::scale_colour_manual(
      values = palette,
      limits = biomarker_levels,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = palette,
      limits = biomarker_levels,
      drop = FALSE
    ) +
    epikinetics_axis_scales(scale) +
    theme_epikinetics()
}

add_censoring_limits <- function(plot, limits) {
  if (is.null(limits) || !nrow(limits)) return(plot)
  value <- biomarker <- NULL
  key <- interaction(
    limits$bound,
    format(limits$value, digits = 17),
    drop = TRUE
  )
  repeated <- stats::ave(seq_len(nrow(limits)), key, FUN = length) > 1L
  shared <- limits[repeated, , drop = FALSE]
  shared <- shared[!duplicated(key[repeated]), , drop = FALSE]
  specific <- limits[!repeated, , drop = FALSE]
  if (nrow(shared)) {
    plot <- plot + ggplot2::geom_hline(
      data = shared,
      mapping = ggplot2::aes(yintercept = value),
      inherit.aes = FALSE,
      colour = "grey45",
      linetype = "dashed",
      linewidth = 0.35,
      alpha = 0.55
    )
  }
  if (nrow(specific)) {
    plot <- plot + ggplot2::geom_hline(
      data = specific,
      mapping = ggplot2::aes(yintercept = value, colour = biomarker),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.35,
      alpha = 0.55,
      show.legend = FALSE
    )
  }
  plot
}

add_epikinetics_observations <- function(plot, observations) {
  if (is.null(observations) || !nrow(observations)) return(plot)
  time_since_exposure <- .plot_value <- biomarker <- censoring <- NULL
  plot +
    ggplot2::geom_point(
      data = observations,
      mapping = ggplot2::aes(
        x = time_since_exposure,
        y = .plot_value,
        colour = biomarker,
        fill = biomarker,
        shape = censoring
      ),
      inherit.aes = FALSE,
      alpha = 0.52,
      size = 1.35,
      stroke = 0.45
    ) +
    ggplot2::scale_shape_manual(
      values = c(none = 16, left = 25, right = 24),
      labels = c(
        none = "Observed",
        left = "Left-censored",
        right = "Right-censored"
      ),
      drop = FALSE
    )
}

#' Plot prepared epikinetics observations
#'
#' @param x An `epikinetics_data` or `epikinetics_fit` object.
#' @param scale Plot values on the response or model log2 scale. Response-scale
#'   axes use log2 spacing, retaining natural-scale labels while respecting the
#'   multiplicative nature of titre measurements.
#' @param ... Reserved for future methods.
#' @return A `ggplot` object.
#' @export
plot_epikinetics_data <- function(x, scale = c("response", "model"), ...) {
  scale <- match.arg(scale)
  prepared <- if (inherits(x, "epikinetics_fit")) x$prepared else x
  if (!inherits(prepared, "epikinetics_data")) {
    stop("'x' must be an epikinetics_data or epikinetics_fit object.",
         call. = FALSE)
  }
  biomarkers <- prepared$mappings$biomarkers
  data <- prediction_observations(prepared, scale, NULL, biomarkers)
  limits <- prediction_censoring_limits(prepared, scale, biomarkers)
  time_since_exposure <- .plot_value <- biomarker <- censoring <- NULL

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = time_since_exposure,
      y = .plot_value,
      colour = biomarker,
      fill = biomarker,
      shape = censoring
    )
  ) +
    ggplot2::geom_point(alpha = 0.52, size = 1.4, stroke = 0.45) +
    ggplot2::scale_shape_manual(
      values = c(none = 16, left = 25, right = 24),
      labels = c(none = "Observed", left = "Left-censored",
                 right = "Right-censored"),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Time since exposure",
      y = if (scale == "response") {
        "Biomarker value"
      } else {
        "Biomarker value (model scale)"
      },
      colour = "Biomarker",
      fill = "Biomarker",
      shape = "Observation"
    )
  plot <- add_censoring_limits(plot, limits)
  add_epikinetics_scales(plot, biomarkers, scale)
}

#' @export
plot.epikinetics_data <- function(x, ...) plot_epikinetics_data(x, ...)

format_profile_value <- function(x) {
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return(as.character(x))
  }
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(format(x))
  format(x, trim = TRUE, scientific = FALSE)
}

prediction_profile_label <- function(x, variables) {
  if ("participant" %in% names(x)) {
    return(as.character(x$participant))
  }
  variables <- intersect(variables, names(x))
  if (!length(variables)) {
    return(rep("Population", nrow(x)))
  }
  values <- lapply(variables, function(variable) {
    paste0(variable, "=", format_profile_value(x[[variable]]))
  })
  do.call(paste, c(values, sep = ", "))
}

prediction_panel_label <- function(x, variables) {
  variables <- intersect(variables, names(x))
  if (!length(variables)) return(rep("Population", nrow(x)))
  values <- lapply(variables, function(variable) {
    value <- format_profile_value(x[[variable]])
    if (length(variables) == 1L) value else paste0(variable, "=", value)
  })
  do.call(paste, c(values, sep = "\n"))
}

conditional_prediction_label <- function(x, data) {
  continuous <- intersect(attr(x, "continuous_covariates"), names(data))
  if (!length(continuous) ||
      !identical(attr(x, "continuous_rule"), "median")) {
    return(NULL)
  }
  values <- vapply(continuous, function(variable) {
    unique_value <- unique(data[[variable]])
    if (length(unique_value) != 1L) return(NA_character_)
    paste0(variable, "=", format_profile_value(unique_value))
  }, character(1))
  values <- values[!is.na(values)]
  if (!length(values)) NULL else paste0(
    "Conditional at participant-level median: ", paste(values, collapse = ", ")
  )
}

#' Plot posterior trajectory predictions
#'
#' Uses the labelled output of [predict.epikinetics_fit()] directly. Biomarkers
#' are overlaid using colour and fill; categorical covariate profiles determine
#' facets. Multiple categorical variables are combined into readable labels.
#' Individual predictions use one panel per participant.
#'
#' @param x An `epikinetics_prediction` data frame.
#' @param central Plot the posterior `"median"` (default) or `"mean"` as the
#'   central trajectory. Both are always retained in summarised predictions.
#' @param show_observations Show stored observations for individual predictions.
#' @param ... Reserved for future methods.
#' @return An ordinary `ggplot` object.
#' @export
plot.epikinetics_prediction <- function(
    x,
    central = c("median", "mean"),
    show_observations = TRUE,
    ...) {
  central <- match.arg(central)
  formula_variables <- attr(x, "formula_variables")
  categorical_covariates <- attr(x, "categorical_covariates")
  data <- as.data.frame(x)
  type <- attr(x, "type")
  biomarker_levels <- attr(x, "biomarker_order")
  if (is.null(biomarker_levels)) {
    biomarker_levels <- unique(as.character(data$biomarker))
  }
  data$biomarker <- factor(as.character(data$biomarker), biomarker_levels)
  data$.profile_line <- prediction_profile_label(data, formula_variables)
  if (identical(type, "individual")) {
    panel <- as.character(data$participant)
    data$.prediction_panel <- factor(panel, levels = unique(panel))
  } else if (length(categorical_covariates)) {
    panel <- prediction_panel_label(data, categorical_covariates)
    data$.prediction_panel <- factor(panel, levels = unique(panel))
  }
  data$.trajectory_group <- interaction(
    data$biomarker,
    data$.profile_line,
    drop = TRUE
  )
  time <- estimate <- lower <- upper <- biomarker <- .profile_line <- NULL
  .prediction_panel <- .trajectory_group <- .central_value <- NULL

  summarised <- isTRUE(attr(x, "summarised"))
  multiple_profiles <- FALSE
  if (summarised) {
    if (!central %in% names(data)) {
      stop("Summarised predictions do not contain '", central, "'.",
           call. = FALSE)
    }
    data$.central_value <- data[[central]]
  }

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = time,
      colour = biomarker,
      fill = biomarker,
      group = .trajectory_group
    )
  )
  if (summarised) {
    multiple_profiles <- any(vapply(
      split(data$.profile_line,
            if (".prediction_panel" %in% names(data)) {
              data$.prediction_panel
            } else {
              rep("all", nrow(data))
            }),
      function(value) length(unique(value)) > 1L,
      logical(1)
    ))
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper),
        alpha = 0.18,
        colour = NA
      )
    if (multiple_profiles) {
      plot <- plot + ggplot2::geom_line(
        ggplot2::aes(y = .central_value, linetype = .profile_line),
        linewidth = 0.85
      )
    } else {
      plot <- plot + ggplot2::geom_line(
        ggplot2::aes(y = .central_value),
        linewidth = 0.85
      )
    }
  } else {
    data$.line_group <- interaction(
      data$.draw,
      data$.profile_line,
      data$biomarker,
      drop = TRUE
    )
    .line_group <- NULL
    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = time,
        y = estimate,
        group = .line_group,
        colour = biomarker
      )
    ) +
      ggplot2::geom_line(alpha = 0.08, linewidth = 0.3)
  }

  probability <- attr(x, "probs")
  interval_label <- if (summarised &&
                        length(probability) == 2L) {
    paste0(
      round(100 * diff(probability)),
      "% interval: ",
      attr(x, "uncertainty")
    )
  } else {
    NULL
  }
  subtitle <- c(interval_label, conditional_prediction_label(x, data))
  subtitle <- paste(subtitle[nzchar(subtitle)], collapse = "\n")
  if (!nzchar(subtitle)) subtitle <- NULL

  if (".prediction_panel" %in% names(data)) {
    n_panels <- length(unique(data$.prediction_panel))
    if (n_panels > 12L) {
      warning(
        "The automatic plot contains ", n_panels,
        " categorical profile panels. Supply a smaller 'newdata' grid for ",
        "a more readable plot.",
        call. = FALSE
      )
    }
    plot <- plot + ggplot2::facet_wrap(
      ggplot2::vars(.prediction_panel)
    )
  }

  plot <- add_censoring_limits(plot, attr(x, "censoring_limits"))
  observations <- attr(x, "observations")
  if (isTRUE(show_observations) && !is.null(observations)) {
    if (identical(type, "individual")) {
      observations$.prediction_panel <- factor(
        as.character(observations$participant),
        levels = levels(data$.prediction_panel)
      )
    }
    plot <- add_epikinetics_observations(plot, observations)
  }
  labels <- list(
    x = "Time since exposure",
    y = if (identical(attr(x, "scale"), "response")) {
      "Biomarker value"
    } else {
      "Biomarker value (model scale)"
    },
    subtitle = subtitle,
    colour = "Biomarker",
    fill = "Biomarker"
  )
  if (multiple_profiles) labels$linetype <- "Profile"
  if (isTRUE(show_observations) && !is.null(observations)) {
    labels$shape <- "Observation"
  }
  plot <- plot + do.call(ggplot2::labs, labels)
  add_epikinetics_scales(plot, biomarker_levels, attr(x, "scale"))
}

#' Plot an epikinetics fit
#'
#' @param x An `epikinetics_fit` object.
#' @param type Plot posterior population trajectories or prepared input data.
#' @param newdata,times,ndraws,probs Passed to [predict.epikinetics_fit()].
#' @param scale Prediction scale.
#' @param central Plot the posterior median or mean trajectory.
#' @param biomarkers Optional biomarker subset.
#' @param show_data Overlay observations for the conditional population plot.
#' @param ... Additional arguments reserved for plot methods.
#' @return A `ggplot` object.
#' @export
plot.epikinetics_fit <- function(
    x,
    type = c("population", "data"),
    newdata = NULL,
    times = 0:150,
    ndraws = 500,
    probs = c(0.025, 0.975),
    scale = c("response", "model"),
    central = c("median", "mean"),
    biomarkers = NULL,
    show_data = is.null(newdata),
    ...) {
  type <- match.arg(type)
  scale <- match.arg(scale)
  central <- match.arg(central)
  if (type == "data") {
    return(plot_epikinetics_data(x, scale = scale))
  }

  prediction <- stats::predict(
    x,
    newdata = newdata,
    times = times,
    type = "population",
    summary = TRUE,
    ndraws = ndraws,
    probs = probs,
    scale = scale,
    biomarkers = biomarkers
  )
  plot <- plot(prediction, central = central, show_observations = FALSE)
  if (isTRUE(show_data)) {
    observed <- prediction_observations(
      x$prepared,
      scale,
      participants = NULL,
      biomarkers = unique(as.character(prediction$biomarker))
    )
    categorical_covariates <- attr(prediction, "categorical_covariates")
    if (length(categorical_covariates)) {
      predicted_panels <- unique(prediction_panel_label(
        prediction,
        categorical_covariates
      ))
      observed$.prediction_panel <- factor(
        prediction_panel_label(observed, categorical_covariates),
        levels = predicted_panels
      )
      observed <- observed[!is.na(observed$.prediction_panel), , drop = FALSE]
    }
    plot <- add_epikinetics_observations(plot, observed) +
      ggplot2::labs(shape = "Observation")
  }
  plot
}

#' Plot fitted individual trajectories
#'
#' Generates (or reuses) fitted-participant latent trajectory summaries and
#' combines them with observations, censoring indicators, and assay limits.
#'
#' @param x An `epikinetics_fit` or individual `epikinetics_prediction` object.
#' @param participant One fitted participant id.
#' @param times,ndraws,probs,scale,biomarkers Passed to [predict()]. Ignored
#'   when `x` is already a prediction object.
#' @param central Plot the posterior median or mean trajectory.
#' @param ... Reserved for future methods.
#' @return An ordinary `ggplot` object.
#' @export
plot_individual <- function(
    x,
    participant,
    times = 0:150,
    ndraws = 500,
    probs = c(0.025, 0.975),
    scale = c("response", "model"),
    biomarkers = NULL,
    central = c("median", "mean"),
    ...) {
  central <- match.arg(central)
  if (missing(participant) || length(participant) != 1L || is.na(participant)) {
    stop("Supply one fitted 'participant' id.", call. = FALSE)
  }
  if (inherits(x, "epikinetics_fit")) {
    scale <- match.arg(scale)
    prediction <- stats::predict(
      x,
      type = "individual",
      participants = participant,
      biomarkers = biomarkers,
      times = times,
      ndraws = ndraws,
      probs = probs,
      scale = scale
    )
  } else if (inherits(x, "epikinetics_prediction") &&
             identical(attr(x, "type"), "individual")) {
    prediction <- subset_individual_prediction(x, participant)
  } else {
    stop("'x' must be an epikinetics_fit or individual prediction object.",
         call. = FALSE)
  }
  plot(prediction, central = central, show_observations = TRUE)
}

subset_individual_prediction <- function(x, participant) {
  keep <- x$participant %in% participant
  if (!any(keep)) {
    stop("Participant '", participant, "' is not present in the prediction.",
         call. = FALSE)
  }
  attributes_to_keep <- attributes(x)
  out <- as.data.frame(x)[keep, , drop = FALSE]
  for (name in setdiff(names(attributes_to_keep), c("names", "row.names", "class"))) {
    attr(out, name) <- attributes_to_keep[[name]]
  }
  observations <- attr(x, "observations")
  if (!is.null(observations)) {
    attr(out, "observations") <- observations[
      observations$participant %in% participant, , drop = FALSE
    ]
  }
  class(out) <- c("epikinetics_prediction", "data.frame")
  rownames(out) <- NULL
  out
}

safe_participant_filename <- function(x) {
  out <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  out <- gsub("^[_.]+|[_.]+$", "", out)
  out[!nzchar(out)] <- "participant"
  make.unique(out, sep = "-")
}

check_output_files <- function(paths, overwrite) {
  existing <- paths[file.exists(paths)]
  if (length(existing) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists: ", existing[1L],
      if (length(existing) > 1L) " (and others)" else "",
      ". Set 'overwrite = TRUE' to replace it.",
      call. = FALSE
    )
  }
}

#' Save fitted individual trajectory plots
#'
#' Computes individual trajectory summaries once, then reuses them for each
#' participant figure. PNG and one-file-per-participant PDF output are
#' supported, as is a single multi-page PDF.
#'
#' @param x An `epikinetics_fit` or summarised individual prediction object.
#' @param path Output directory, created if necessary.
#' @param participants Optional participant subset.
#' @param format `"png"` or `"pdf"`.
#' @param multipage For PDF output, write one multi-page file rather than one
#'   file per participant.
#' @param file Filename for a multi-page PDF.
#' @param times,ndraws,probs,scale,biomarkers Passed to [predict()] when `x` is
#'   a fit.
#' @param central Plot the posterior median or mean trajectory.
#' @param width,height Figure dimensions in inches.
#' @param dpi Raster resolution for PNG output.
#' @param overwrite Replace existing output files.
#' @return The output paths, invisibly.
#' @export
save_individual_plots <- function(
    x,
    path,
    participants = NULL,
    format = c("png", "pdf"),
    multipage = FALSE,
    file = "individual-trajectories.pdf",
    times = 0:150,
    ndraws = 500,
    probs = c(0.025, 0.975),
    scale = c("response", "model"),
    biomarkers = NULL,
    central = c("median", "mean"),
    width = 7.2,
    height = 4.8,
    dpi = 300,
    overwrite = FALSE) {
  format <- match.arg(format)
  central <- match.arg(central)
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("'path' must be one non-empty directory path.", call. = FALSE)
  }
  if (!is.logical(multipage) || length(multipage) != 1L || is.na(multipage) ||
      !is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("'multipage' and 'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (multipage && format != "pdf") {
    stop("'multipage = TRUE' is only available for PDF output.",
         call. = FALSE)
  }
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    stop("Could not create output directory '", path, "'.", call. = FALSE)
  }

  prediction <- if (inherits(x, "epikinetics_fit")) {
    scale <- match.arg(scale)
    stats::predict(
      x,
      type = "individual",
      participants = participants,
      biomarkers = biomarkers,
      times = times,
      ndraws = ndraws,
      probs = probs,
      scale = scale,
      summary = TRUE
    )
  } else if (inherits(x, "epikinetics_prediction") &&
             identical(attr(x, "type"), "individual") &&
             isTRUE(attr(x, "summarised"))) {
    x
  } else {
    stop("'x' must be an epikinetics_fit or summarised individual prediction.",
         call. = FALSE)
  }
  available <- unique(prediction$participant)
  if (is.null(participants)) {
    participants <- available
  } else {
    unknown <- setdiff(participants, available)
    if (length(unknown)) {
      stop("Participant(s) absent from predictions: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    participants <- available[available %in% participants]
  }

  if (multipage) {
    output <- file.path(path, file)
    check_output_files(output, overwrite)
    grDevices::pdf(output, width = width, height = height, onefile = TRUE)
    device_open <- TRUE
    on.exit(if (device_open) grDevices::dev.off(), add = TRUE)
    for (participant in participants) {
      print(plot_individual(prediction, participant, central = central))
    }
    grDevices::dev.off()
    device_open <- FALSE
    return(invisible(output))
  }

  stems <- safe_participant_filename(participants)
  outputs <- file.path(path, paste0(stems, ".", format))
  check_output_files(outputs, overwrite)
  for (i in seq_along(participants)) {
    figure <- plot_individual(
      prediction,
      participants[i],
      central = central
    )
    ggplot2::ggsave(
      filename = outputs[i],
      plot = figure,
      width = width,
      height = height,
      dpi = if (format == "png") dpi else 300,
      units = "in",
      device = format
    )
  }
  invisible(outputs)
}

rpositive_normal <- function(n, mean, sd) {
  lower_probability <- stats::pnorm(0, mean = mean, sd = sd)
  probability <- stats::runif(n, min = lower_probability, max = 1)
  probability <- pmin(probability, 1 - .Machine$double.eps)
  stats::qnorm(probability, mean = mean, sd = sd)
}

#' Plot prior kinetic trajectories
#'
#' Draw population-level kinetic curves from the configured priors. The band
#' shows the pointwise 95% prior interval and the line shows the pointwise
#' prior median.
#'
#' @param x An `epikinetics_priors` object.
#' @param ... Reserved for compatibility with the [plot()] generic.
#' @param times Non-negative prediction times.
#' @param ndraws Number of prior trajectories to simulate.
#' @param reference_value Positive reference value used to convert model-scale
#'   values to the response scale when `scale = "response"`.
#' @param scale Output scale: `"model"` for log2-relative values or
#'   `"response"` for natural measurement values.
#'
#' @return A [ggplot2::ggplot()] object.
#' @rdname epikinetics_priors
#' @export
plot.epikinetics_priors <- function(
    x,
    ...,
    times = 0:150,
    ndraws = 1000,
    reference_value = 1,
    scale = c("model", "response")) {
  scale <- match.arg(scale)
  ndraws <- validate_count(ndraws, "ndraws")
  if (!is.numeric(times) || !length(times) || any(!is.finite(times)) ||
      any(times < 0)) {
    stop("'times' must be finite and non-negative.", call. = FALSE)
  }

  population <- x$population
  parameters <- data.frame(
    baseline = stats::rnorm(
      ndraws,
      population$baseline["mean"],
      population$baseline["sd"]
    ),
    time_to_peak = rpositive_normal(
      ndraws,
      population$time_to_peak["mean"],
      population$time_to_peak["sd"]
    ),
    waning_duration = rpositive_normal(
      ndraws,
      population$waning_duration["mean"],
      population$waning_duration["sd"]
    ),
    boost_rate = rpositive_normal(
      ndraws,
      population$boost_rate["mean"],
      population$boost_rate["sd"]
    ),
    early_waning_rate = rpositive_normal(
      ndraws,
      population$early_waning_rate["mean"],
      population$early_waning_rate["sd"]
    ),
    late_waning_rate = rpositive_normal(
      ndraws,
      population$late_waning_rate["mean"],
      population$late_waning_rate["sd"]
    )
  )
  rows <- rep(seq_len(ndraws), each = length(times))
  time <- rep(as.numeric(times), times = ndraws)
  expanded <- parameters[rows, , drop = FALSE]
  estimate <- kinetics_mean(
    time,
    expanded$baseline,
    expanded$time_to_peak,
    expanded$time_to_peak + expanded$waning_duration,
    expanded$boost_rate,
    expanded$early_waning_rate,
    expanded$late_waning_rate
  )
  if (scale == "response") estimate <- reference_value * 2^estimate
  draws <- data.frame(.draw = rows, time = time, estimate = estimate)
  prediction <- summarise_prediction_draws(draws, c(0.025, 0.975))
  time <- estimate <- lower <- upper <- NULL
  ggplot2::ggplot(prediction, ggplot2::aes(x = time, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time since exposure",
      y = if (scale == "response") "Biomarker value" else "Biomarker value (model scale)"
    )
}
