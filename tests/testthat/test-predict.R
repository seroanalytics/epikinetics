test_that("the three kinetic segments join continuously", {
  time <- c(0, 10, 60, 100)
  mean <- epikinetics:::kinetics_mean(
    time,
    baseline = rep(6, 4),
    time_to_peak = rep(10, 4),
    waning_change_time = rep(60, 4),
    boost_rate = rep(0.25, 4),
    early_waning_rate = rep(0.02, 4),
    late_waning_rate = rep(0.002, 4)
  )
  expect_equal(mean, c(6, 8.5, 7.5, 7.42))
})

test_that("population predictions return draws or uncertainty summaries", {
  fit <- fake_epikinetics_fit()
  draws <- predict(fit, times = c(0, 10, 60), summary = FALSE, ndraws = 5)
  expect_s3_class(draws, "epikinetics_prediction")
  expect_length(unique(draws$.draw), 5)
  expect_equal(range(draws$.draw), c(1L, 20L))
  expect_true(all(draws$estimate > 0))

  summary <- predict(fit, times = c(0, 10, 60), ndraws = 5)
  expect_true(all(
    c("estimate", "mean", "median", "lower", "upper") %in% names(summary)
  ))
  expect_equal(summary$estimate, summary$median)
  expect_equal(sort(unique(summary$time)), c(0, 10, 60))
  expect_true(all(summary$upper > summary$lower))
  expect_identical(attr(summary, "uncertainty"), "latent expected trajectory")
})

test_that("prediction preserves draws and calculates intervals across draws", {
  fit <- fake_epikinetics_fit(ndraws = 20)
  draws <- predict(
    fit,
    times = c(0, 10, 60),
    summary = FALSE,
    scale = "model"
  )
  intervals <- predict(
    fit,
    times = c(0, 10, 60),
    summary = TRUE,
    scale = "model"
  )

  expect_length(unique(draws$.draw), 20)
  expect_gt(length(unique(draws$estimate)), 20)
  for (row in seq_len(nrow(intervals))) {
    values <- draws$estimate[
      draws$biomarker == intervals$biomarker[row] &
        draws$time == intervals$time[row] &
        draws$.profile == intervals$.profile[row]
    ]
    expect_equal(intervals$lower[row], unname(quantile(values, 0.025)))
    expect_equal(intervals$upper[row], unname(quantile(values, 0.975)))
    expect_equal(intervals$mean[row], mean(values))
    expect_equal(intervals$median[row], median(values))
  }
})

test_that("latent and posterior predictive uncertainty are labelled", {
  fit <- fake_epikinetics_fit()
  latent <- predict(fit, times = 10, ndraws = 20)
  observed <- predict(
    fit,
    times = 10,
    ndraws = 20,
    include_observation_noise = TRUE,
    seed = 42
  )
  expect_identical(attr(latent, "uncertainty"), "latent expected trajectory")
  expect_identical(
    attr(observed, "uncertainty"),
    "posterior predictive observation"
  )
  expect_true(any((observed$upper - observed$lower) >
                  (latent$upper - latent$lower)))
})

test_that("new covariate profiles use the stored formula contrasts", {
  fit <- fake_epikinetics_fit()
  profiles <- data.frame(group = c("reference", "treated"))
  prediction <- predict(
    fit,
    newdata = profiles,
    times = 0,
    summary = FALSE,
    ndraws = 3
  )
  reference <- prediction$estimate[prediction$group == "reference"]
  treated <- prediction$estimate[prediction$group == "treated"]
  expect_true(all(treated > reference))
  expect_error(
    predict(fit, newdata = data.frame(group = "unknown"), times = 0),
    "Unknown factor level.*group.*unknown"
  )
})

test_that("custom factor contrasts are retained for new profiles", {
  data <- example_epikinetics_data()
  data$group <- factor(data$group)
  stats::contrasts(data$group) <- stats::contr.sum(2)
  prepared <- prepare_epikinetics_data(data, formula = ~ group)

  expect_equal(
    unname(prepared$mappings$contrast_matrices$group),
    unname(stats::contr.sum(2))
  )
  expect_true(is.na(prepared$mappings$reference_levels[["group"]]))
  design <- epikinetics:::build_prediction_design(
    prepared,
    data.frame(group = c("reference", "treated"))
  )
  expect_equal(unname(design$matrix[, 1]), c(1, -1))
})

test_that("default grids have no artificial strata without covariates", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ 1
  )
  grid <- prediction_grid(prepared)
  expect_equal(names(grid), ".profile")
  expect_equal(grid$.profile, 1L)

  prediction <- predict(
    fake_epikinetics_fit(prepared),
    times = 0,
    summary = FALSE,
    ndraws = 2
  )
  expect_equal(unique(prediction$.profile), 1L)
  expect_length(attr(prediction, "categorical_covariates"), 0L)
})

test_that("default grids expose observed categorical levels", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group
  )
  grid <- prediction_grid(prepared)
  expect_equal(as.character(grid$group), c("reference", "treated"))

  prediction <- predict(
    fake_epikinetics_fit(prepared),
    times = 0,
    summary = FALSE,
    ndraws = 2
  )
  expect_equal(
    unique(as.character(prediction$group)),
    c("reference", "treated")
  )
  expect_identical(attr(prediction, "categorical_rule"), "observed")
})

test_that("continuous predictors default to the median and accept newdata", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ age
  )
  expect_equal(prediction_grid(prepared)$age, 40)
  expect_equal(prediction_grid(prepared, continuous = "mean")$age, 40)

  fit <- fake_epikinetics_fit(prepared)
  default <- predict(fit, times = 0, summary = FALSE, ndraws = 2)
  explicit <- predict(
    fit,
    newdata = data.frame(age = 35),
    times = 0,
    summary = FALSE,
    ndraws = 2
  )
  expect_equal(unique(default$age), 40)
  expect_equal(unique(explicit$age), 35)
})

test_that("multiple factors use observed combinations unless requested", {
  data <- example_epikinetics_data()
  data$sex <- rep(c("F", "M", "F"), each = 6)
  prepared <- prepare_epikinetics_data(data, formula = ~ group + sex)

  observed <- prediction_grid(prepared)
  cartesian <- prediction_grid(prepared, categorical = "cartesian")
  expect_equal(nrow(observed), 3L)
  expect_equal(nrow(cartesian), 4L)
  expect_false(any(
    observed$group == "reference" & as.character(observed$sex) == "M"
  ))
  expect_true(any(
    cartesian$group == "reference" & as.character(cartesian$sex) == "M"
  ))
})

test_that("interactions and transformed terms use the stored R formula", {
  data <- example_epikinetics_data()
  data$sex <- rep(c("F", "M", "F"), each = 6)
  extra <- data[data$pid == "P-01", ]
  extra$pid <- "P-04"
  extra$sex <- "M"
  data <- rbind(data, extra)
  interaction_data <- prepare_epikinetics_data(
    data,
    formula = ~ group * sex
  )
  design <- epikinetics:::build_prediction_design(interaction_data)
  expect_identical(
    colnames(design$matrix),
    interaction_data$mappings$covariates
  )
  expect_true(any(grepl(":", interaction_data$mappings$design_columns$term)))

  transformed <- prepare_epikinetics_data(data, formula = ~ log(age))
  transformed_design <- epikinetics:::build_prediction_design(transformed)
  expect_equal(unname(transformed_design$matrix[1, ]), log(median(c(30, 40, 50, 30))))
})

test_that("individual and new participant predictions are distinct", {
  fit <- fake_epikinetics_fit()
  all_fitted <- predict(
    fit,
    type = "individual",
    times = c(0, 3, 17),
    ndraws = 4
  )
  expect_equal(unique(all_fitted$participant), c("P-01", "P-02", "P-03"))
  expect_equal(sort(unique(all_fitted$time)), c(0, 3, 17))
  expect_true(all(all_fitted$upper > all_fitted$lower))

  fitted <- predict(
    fit,
    type = "participant",
    participants = "P-02",
    times = c(0, 10),
    summary = FALSE,
    ndraws = 4
  )
  expect_equal(unique(fitted$participant), "P-02")
  expect_identical(attr(fitted, "type"), "individual")

  new_one <- predict(
    fit,
    type = "new",
    newdata = data.frame(group = "treated"),
    times = c(0, 10),
    summary = FALSE,
    ndraws = 4,
    seed = 42
  )
  new_two <- predict(
    fit,
    type = "new",
    newdata = data.frame(group = "treated"),
    times = c(0, 10),
    summary = FALSE,
    ndraws = 4,
    seed = 42
  )
  expect_equal(new_one, new_two)
})

test_that("individual prediction subsets biomarkers and protects memory", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group,
    biomarker_order = c("B", "A")
  )
  fit <- fake_epikinetics_fit(prepared)
  prediction <- predict(
    fit,
    type = "individual",
    participants = c("P-03", "P-01"),
    biomarkers = "A",
    times = c(2, 11),
    ndraws = 5
  )
  expect_equal(unique(prediction$participant), c("P-01", "P-03"))
  expect_equal(unique(as.character(prediction$biomarker)), "A")
  expect_equal(levels(prediction$biomarker), c("B", "A"))
  expect_equal(sort(unique(prediction$time)), c(2, 11))
  expect_true(all(c("mean", "median", "lower", "upper") %in% names(prediction)))

  expect_error(
    predict(
      fit,
      type = "individual",
      summary = FALSE,
      times = 0:10,
      max_rows = 10
    ),
    "above 'max_rows'"
  )
  expect_error(
    predict(fit, biomarkers = "unknown", times = 0),
    "Unknown biomarker"
  )
})

test_that("biomarker order propagates through predictions", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group,
    biomarker_order = c("B", "A")
  )
  prediction <- predict(
    fake_epikinetics_fit(prepared),
    times = c(0, 10),
    ndraws = 4
  )
  expect_equal(levels(prediction$biomarker), c("B", "A"))
  expect_equal(attr(prediction, "biomarker_order"), c("B", "A"))
})

test_that("model and response scales map through the reference value", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group,
    reference_value = 5
  )
  fit <- fake_epikinetics_fit(prepared)
  model <- predict(fit, times = 0, scale = "model", summary = FALSE, ndraws = 2)
  response <- predict(fit, times = 0, scale = "response", summary = FALSE, ndraws = 2)
  expect_equal(response$estimate, 5 * 2^model$estimate)
})

test_that("plot metadata converts log2 inputs and assay limits to response scale", {
  data <- example_epikinetics_data()
  natural_values <- data$value
  data$value <- log2(data$value)
  prepared <- prepare_epikinetics_data(
    data,
    formula = ~ group,
    scale = "log2",
    lower_limit = log2(c(A = 5, B = 10)),
    upper_limit = log2(c(A = 1000, B = 2000))
  )
  prediction <- predict(
    fake_epikinetics_fit(prepared),
    times = 0,
    ndraws = 3,
    scale = "response"
  )
  limits <- attr(prediction, "censoring_limits")
  expect_equal(
    sort(limits$value[limits$bound == "lower"]),
    c(5, 10),
    tolerance = 1e-12
  )
  expect_equal(
    sort(limits$value[limits$bound == "upper"]),
    c(1000, 2000),
    tolerance = 1e-12
  )

  individual <- predict(
    fake_epikinetics_fit(prepared),
    type = "individual",
    participants = "P-01",
    times = 0,
    ndraws = 3,
    scale = "response"
  )
  observed <- attr(individual, "observations")
  expected <- natural_values[example_epikinetics_data()$pid == "P-01"]
  expect_equal(sort(observed$.plot_value), sort(expected))
})
