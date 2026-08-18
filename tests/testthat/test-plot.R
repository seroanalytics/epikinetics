test_that("data, prior, prediction, and fit plots are ordinary ggplots", {
  prepared <- prepare_epikinetics_data(example_epikinetics_data())
  fit <- fake_epikinetics_fit()
  prediction <- predict(fit, times = 0:5, ndraws = 5)

  expect_s3_class(plot(prepared), "ggplot")
  expect_s3_class(plot(epikinetics_priors(), times = 0:5, ndraws = 20), "ggplot")
  expect_s3_class(plot(prediction), "ggplot")
  expect_s3_class(plot(fit, times = 0:5, ndraws = 5), "ggplot")
})

test_that("response-scale prior trajectories use the package log2 axis", {
  set.seed(10)
  response <- plot(
    epikinetics_priors(),
    times = 0:20,
    ndraws = 100,
    probs = c(0.05, 0.95),
    scale = "response"
  )
  model <- plot(
    epikinetics_priors(),
    times = 0:20,
    ndraws = 100,
    scale = "model"
  )

  expect_identical(response$scales$get_scales("y")$trans$name, "log-2")
  expect_identical(model$scales$get_scales("y")$trans$name, "identity")
  expect_match(response$labels$subtitle, "90% pointwise prior interval")
  expect_match(response$labels$subtitle, "observation error are excluded")
  expect_error(
    plot(epikinetics_priors(), probs = c(0.95, 0.05)),
    "ordered probabilities"
  )
  expect_error(
    plot(epikinetics_priors(), reference_value = 0),
    "finite positive"
  )
})

test_that("unsummarised posterior trajectories can be plotted", {
  prediction <- predict(
    fake_epikinetics_fit(),
    times = 0:5,
    summary = FALSE,
    ndraws = 3
  )
  expect_s3_class(plot(prediction), "ggplot")
})

test_that("trajectory ribbons and central lines use returned summaries", {
  prediction <- predict(
    fake_epikinetics_fit(),
    times = c(0, 10, 60),
    ndraws = 20,
    scale = "model"
  )
  median_built <- ggplot2::ggplot_build(plot(prediction))
  mean_built <- ggplot2::ggplot_build(plot(prediction, central = "mean"))
  ribbon <- median_built$data[[1L]]
  median_line <- median_built$data[[2L]]
  mean_line <- mean_built$data[[2L]]

  expect_equal(sort(ribbon$ymin), sort(prediction$lower))
  expect_equal(sort(ribbon$ymax), sort(prediction$upper))
  expect_equal(sort(median_line$y), sort(prediction$median))
  expect_equal(sort(mean_line$y), sort(prediction$mean))
  expect_true(all(ribbon$ymax > ribbon$ymin))
  expect_match(plot(prediction)$labels$subtitle, "latent expected trajectory")
})

test_that("population plots overlay biomarkers and facet by formula profiles", {
  prediction <- predict(
    fake_epikinetics_fit(),
    times = c(0, 10),
    ndraws = 5,
    scale = "model"
  )
  figure <- plot(prediction)
  built <- ggplot2::ggplot_build(figure)
  layout <- built$layout$layout

  expect_setequal(
    as.character(layout$.prediction_panel),
    c("reference", "treated")
  )
  expect_equal(nrow(layout), 2L)
  expect_false("biomarker" %in% names(layout))
  expect_equal(length(unique(built$data[[2L]]$colour)), 2L)
  expect_equal(figure$labels$colour, "Biomarker")
  expect_equal(figure$theme$legend.position, "bottom")
})

test_that("faceting is formula-aware rather than hard-coded", {
  data <- example_epikinetics_data()
  data$sex <- rep(c("F", "M", "F"), each = 6)
  prepared <- prepare_epikinetics_data(data, formula = ~ group + sex)
  prediction <- predict(
    fake_epikinetics_fit(prepared),
    times = c(0, 10),
    ndraws = 4,
    scale = "model"
  )
  layout <- ggplot2::ggplot_build(plot(prediction))$layout$layout
  expect_setequal(
    as.character(layout$.prediction_panel),
    c("group=reference\nsex=F", "group=treated\nsex=M",
      "group=treated\nsex=F")
  )
})

test_that("censoring limits and censored observations have dedicated layers", {
  fit <- fake_epikinetics_fit()
  prediction <- predict(fit, times = c(0, 10), ndraws = 5)
  figure <- plot(prediction)
  expect_true(any(vapply(
    figure$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  )))

  observed <- plot(fit, times = c(0, 10), ndraws = 5)
  expect_true(any(vapply(
    observed$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  )))
  expect_true(inherits(observed$scales$get_scales("y")$trans, "transform"))
})

test_that("individual plots reuse predictions and retain participant labels", {
  fit <- fake_epikinetics_fit()
  prediction <- predict(
    fit,
    type = "individual",
    participants = c("P-01", "P-02"),
    times = c(0, 10),
    ndraws = 5,
    scale = "model"
  )
  figure <- plot_individual(prediction, "P-02")
  built <- ggplot2::ggplot_build(figure)

  expect_s3_class(figure, "ggplot")
  expect_equal(figure$labels$title, "Participant P-02")
  expect_setequal(
    as.character(built$layout$layout$biomarker),
    c("A", "B")
  )
  expect_equal(nrow(built$layout$layout), 2L)
  expect_true(any(vapply(
    figure$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  )))
  horizontal_lines <- which(vapply(
    figure$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  ))
  expect_true(length(horizontal_lines) >= 1L)
  line_panels <- unique(unlist(lapply(
    ggplot2::ggplot_build(figure)$data[horizontal_lines],
    function(layer) layer$PANEL
  )))
  expect_setequal(as.integer(line_panels), c(1L, 2L))
  expect_error(plot_individual(prediction, "missing"), "not present")

  combined <- plot(prediction, show_observations = FALSE)
  combined_layout <- ggplot2::ggplot_build(combined)$layout$layout
  expect_equal(nrow(combined_layout), 4L)
  expect_setequal(
    as.character(combined_layout$participant),
    c("P-01", "P-02")
  )
})

test_that("batch individual plots support PNG and multi-page PDF", {
  fit <- fake_epikinetics_fit()
  png_path <- tempfile("epikinetics-png-")
  pdf_path <- tempfile("epikinetics-pdf-")

  png <- save_individual_plots(
    fit,
    png_path,
    participants = "P-01",
    times = c(0, 10),
    ndraws = 3,
    format = "png"
  )
  pdf <- save_individual_plots(
    fit,
    pdf_path,
    participants = c("P-01", "P-02"),
    times = c(0, 10),
    ndraws = 3,
    format = "pdf",
    multipage = TRUE
  )
  expect_true(file.exists(png))
  expect_true(file.exists(pdf))
  expect_match(basename(png), "P-01\\.png")
  expect_match(basename(pdf), "individual-trajectories\\.pdf")
})
