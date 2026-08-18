test_that("ordinary data frames are prepared without mutation", {
  data <- example_epikinetics_data()
  original <- data
  prepared <- prepare_epikinetics_data(data, formula = ~ group)

  expect_s3_class(prepared, "epikinetics_data")
  expect_identical(data, original)
  expect_equal(prepared$stan_data$N_observations, nrow(data))
  expect_equal(prepared$stan_data$N_participants, 3)
  expect_equal(prepared$stan_data$N_biomarkers, 2)
  expect_equal(prepared$mappings$participants, c("P-01", "P-02", "P-03"))
  expect_equal(prepared$mappings$biomarkers, c("A", "B"))
  expect_true(all(prepared$observations$time_since_exposure >= 0))
  expect_identical(prepared$input_data, original)
  expect_true(all(c(
    "source_row", "participant_index", "biomarker_index", "value_model",
    "censoring_code", "lower_limit_model", "upper_limit_model"
  ) %in% names(prepared$observations)))
})

test_that("already aligned numeric time can be prepared without an exposure column", {
  data <- example_epikinetics_data()
  data$time_since_exposure <- as.numeric(data$day - data$last_exp_day)
  data$last_exp_day <- NULL

  prepared <- prepare_epikinetics_data(
    data,
    time = "time_since_exposure",
    exposure = NULL
  )

  expect_equal(prepared$observations$time_since_exposure,
               prepared$observations$observation_time)
  expect_true(all(prepared$observations$exposure_time == 0))
  expect_true(all(prepared$participants$exposure_time == 0))
  expect_true(prepared$specification$time_already_aligned)
  expect_null(prepared$specification$columns$exposure)
  expect_output(
    print(prepared),
    "time supplied relative to exposure at zero"
  )

  data$time_since_exposure <- as.Date("2024-01-01")
  expect_error(
    prepare_epikinetics_data(
      data, time = "time_since_exposure", exposure = NULL
    ),
    "must contain numeric time since exposure"
  )

  data$time_since_exposure <- seq_len(nrow(data))
  expect_error(
    prepare_epikinetics_data(
      data, time = "time_since_exposure", exposure = ""
    ),
    "'exposure' must be NULL or one non-empty"
  )
  expect_error(
    prepare_epikinetics_data(data, time = "pid", exposure = NULL),
    "columns must be distinct"
  )

  data$time_since_exposure[1] <- Inf
  expect_error(
    prepare_epikinetics_data(
      data, time = "time_since_exposure", exposure = NULL
    ),
    "Aligned observation times must be finite"
  )
})

test_that("prepared data expose every important Stan input and mapping", {
  data <- example_epikinetics_data()
  prepared <- prepare_epikinetics_data(data, formula = ~ group + age)

  expect_identical(stan_data(prepared), prepared$stan_data)
  expect_equal(model.matrix(prepared), prepared$model_matrix)
  expect_equal(dim(model.matrix(prepared)), c(3, 2))
  expect_equal(rownames(model.matrix(prepared)), c("P-01", "P-02", "P-03"))
  expect_equal(
    prepared$mappings$participant$participant_index,
    seq_len(prepared$stan_data$N_participants)
  )
  expect_equal(
    prepared$mappings$biomarker$biomarker_index,
    seq_len(prepared$stan_data$N_biomarkers)
  )
  expect_equal(prepared$mappings$reference_levels[["group"]], "reference")
  expect_s3_class(prepared$model_frame, "data.frame")
  expect_equal(
    prepared$specification$term_labels,
    c("group", "age")
  )
  expect_equal(
    prepared$mappings$design_columns$term,
    c("group", "age")
  )
  expect_equal(
    prepared$mappings$design_columns$label[1],
    "group=treated (vs reference)"
  )
  expect_equal(
    prepared$specification$categorical_variables,
    "group"
  )
  expect_equal(
    prepared$specification$continuous_variables,
    "age"
  )
  expect_equal(prepared$participants$observation_count, c(6, 6, 6))

  overview <- summary(prepared)
  expect_s3_class(overview, "summary.epikinetics_data")
  expect_equal(unname(overview$counts["observations"]), nrow(data))
  expect_equal(overview$model_matrix_dimensions, c(3, 2))
})

test_that("natural and log2 scales are explicit and invertible", {
  data <- example_epikinetics_data()
  natural <- prepare_epikinetics_data(data, reference_value = 5)
  expect_equal(natural$data$value_model, log2(natural$data$value / 5))

  data$value <- log2(data$value / 5)
  model_scale <- prepare_epikinetics_data(
    data,
    scale = "log2",
    reference_value = 5
  )
  expect_equal(model_scale$data$value_model, model_scale$data$value)
})

test_that("standard formula contrasts are retained and intercept is removed", {
  data <- example_epikinetics_data()
  prepared <- prepare_epikinetics_data(data, formula = ~ group + age)

  expect_equal(
    prepared$mappings$covariates,
    c("grouptreated", "age")
  )
  expect_equal(dim(prepared$stan_data$X), c(3, 2))
  expect_error(
    prepare_epikinetics_data(data, formula = ~ 0 + group),
    "must include an intercept"
  )

  data$group <- factor(data$group, levels = c("treated", "reference"))
  relevelled <- prepare_epikinetics_data(data, formula = ~ group + age)
  expect_equal(relevelled$mappings$reference_levels[["group"]], "treated")
  expect_equal(colnames(model.matrix(relevelled))[1], "groupreference")
})

test_that("covariate effects target explicit kinetic parameters", {
  data <- example_epikinetics_data()
  prepared <- prepare_epikinetics_data(
    data,
    formula = ~ group,
    covariate_parameters = c("baseline", "late_waning_rate")
  )

  expect_equal(
    prepared$mappings$covariate_parameters,
    c("baseline", "late_waning_rate")
  )
  expect_equal(prepared$stan_data$covariate_active, c(1L, 0L, 0L, 0L, 0L, 1L))
  expect_equal(summary(prepared)$covariate_parameters,
               c("baseline", "late_waning_rate"))

  no_covariates <- prepare_epikinetics_data(data, formula = ~ 1)
  expect_equal(no_covariates$stan_data$covariate_active, integer(6))
  expect_error(
    prepare_epikinetics_data(
      data,
      formula = ~ group,
      covariate_parameters = "unknown"
    ),
    "Unknown covariate-effect"
  )
})

test_that("participant random effects are explicit and parsimonious by default", {
  data <- example_epikinetics_data()
  prepared <- prepare_epikinetics_data(data)

  expect_equal(
    prepared$mappings$participant_parameters,
    c(
      "baseline", "boost_rate", "early_waning_rate", "late_waning_rate"
    )
  )
  expect_equal(
    prepared$stan_data$participant_effect_active,
    c(1L, 0L, 0L, 1L, 1L, 1L)
  )
  expect_equal(
    summary(prepared)$participant_parameters,
    c(
      "baseline", "boost_rate", "early_waning_rate", "late_waning_rate"
    )
  )

  full <- prepare_epikinetics_data(data, participant_parameters = "all")
  expect_equal(full$stan_data$participant_effect_active, rep(1L, 6L))

  none <- prepare_epikinetics_data(data, participant_parameters = character())
  expect_equal(none$stan_data$participant_effect_active, integer(6L))

  expect_error(
    prepare_epikinetics_data(data, participant_parameters = "unknown"),
    "Unknown participant-effect"
  )
  expect_error(
    prepare_epikinetics_data(
      data,
      participant_parameters = c("baseline", "baseline")
    ),
    "must not contain duplicates"
  )
})

test_that("biomarker ordering is explicit, stable, and validated", {
  data <- example_epikinetics_data()
  explicit <- prepare_epikinetics_data(
    data,
    biomarker_order = c("B", "A")
  )
  expect_equal(explicit$mappings$biomarkers, c("B", "A"))
  expect_equal(levels(explicit$data$biomarker), c("B", "A"))
  expect_equal(explicit$specification$biomarker_order_source, "explicit")
  expect_equal(unique(as.character(explicit$data$biomarker)), c("B", "A"))
  expect_equal(unique(explicit$stan_data$biomarker), c(1L, 2L))

  data$titre_type <- factor(data$titre_type, levels = c("B", "A"))
  factor_order <- prepare_epikinetics_data(data)
  expect_equal(factor_order$mappings$biomarkers, c("B", "A"))
  expect_equal(
    factor_order$specification$biomarker_order_source,
    "factor levels"
  )

  first_appearance <- prepare_epikinetics_data(example_epikinetics_data())
  expect_equal(first_appearance$mappings$biomarkers, c("A", "B"))
  expect_equal(
    first_appearance$specification$biomarker_order_source,
    "first appearance"
  )

  expect_error(
    prepare_epikinetics_data(data, biomarker_order = "A"),
    "every observed biomarker"
  )
  expect_error(
    prepare_epikinetics_data(data, biomarker_order = c("A", "B", "C")),
    "not observed"
  )
  expect_error(
    prepare_epikinetics_data(data, biomarker_order = c("A", "A")),
    "duplicate"
  )
})

test_that("participant-level assumptions are validated early", {
  data <- example_epikinetics_data()
  data$age[1] <- 99
  expect_error(
    prepare_epikinetics_data(data, formula = ~ age),
    "must be constant within participant"
  )

  data <- example_epikinetics_data()
  data$last_exp_day[1] <- data$last_exp_day[1] - 1
  expect_error(
    prepare_epikinetics_data(data),
    "must be constant within participant"
  )

  data <- example_epikinetics_data()
  data$day[1] <- data$last_exp_day[1] - 1
  expect_error(
    prepare_epikinetics_data(data),
    "on or after"
  )

  data <- example_epikinetics_data()
  data$pid[1] <- NA_character_
  expect_error(prepare_epikinetics_data(data), "IDs must not be missing")

  data <- example_epikinetics_data()
  data$pid[1] <- " "
  expect_error(prepare_epikinetics_data(data), "IDs must not be empty")

  data <- example_epikinetics_data()
  data$group[1] <- NA_character_
  expect_error(
    prepare_epikinetics_data(data, formula = ~ group),
    "Missing covariate"
  )
})

test_that("sparse participants and multiple biomarkers are valid", {
  data <- example_epikinetics_data()
  data <- data[c(1, 7:18), ]
  prepared <- prepare_epikinetics_data(data)
  expect_equal(min(prepared$participants$observation_count), 1)
  expect_equal(prepared$stan_data$N_biomarkers, 2)
  expect_equal(prepared$stan_data$observation_start[1], 1)
  expect_equal(tail(prepared$stan_data$observation_end, 1), nrow(data))
})

test_that("censoring supports none, left, right, and biomarker limits", {
  data <- example_epikinetics_data()
  prepared <- prepare_epikinetics_data(
    data,
    lower_limit = c(A = 5, B = 10),
    upper_limit = c(A = 55, B = 110)
  )
  expect_true(any(prepared$stan_data$censoring == -1L))
  expect_true(any(prepared$stan_data$censoring == 0L))
  expect_true(any(prepared$stan_data$censoring == 1L))

  uncensored <- prepare_epikinetics_data(data)
  expect_true(all(uncensored$stan_data$censoring == 0L))

  lower <- ifelse(data$titre_type == "A", 5, 10)
  upper <- ifelse(data$titre_type == "A", 55, 110)
  data$censor <- ifelse(
    data$value <= lower,
    "left",
    ifelse(data$value >= upper, "right", "none")
  )
  explicit <- prepare_epikinetics_data(
    data,
    lower_limit = lower,
    upper_limit = upper,
    censoring = "censor"
  )
  expect_equal(explicit$stan_data$censoring, prepared$stan_data$censoring)
  all_left <- prepare_epikinetics_data(
    data,
    lower_limit = max(data$value),
    censoring = "left"
  )
  expect_true(all(all_left$stan_data$censoring == -1L))
  expect_error(
    prepare_epikinetics_data(data, censoring = "censor"),
    "requires a lower limit"
  )
})

test_that("row-specific censoring columns are accepted", {
  data <- example_epikinetics_data()
  data$lod <- ifelse(data$titre_type == "A", 5, 10)
  data$uloq <- 1000
  prepared <- prepare_epikinetics_data(
    data,
    lower_limit = "lod",
    upper_limit = "uloq"
  )
  expect_equal(sort(unique(prepared$data$lower_limit)), c(5, 10))
})

test_that("invalid inputs produce targeted errors", {
  data <- example_epikinetics_data()
  expect_error(prepare_epikinetics_data(as.matrix(data)), "data.frame")
  expect_error(
    prepare_epikinetics_data(data[, setdiff(names(data), "value")]),
    "Missing required columns: value"
  )
  data$value[1] <- 0
  expect_error(prepare_epikinetics_data(data), "must all be positive")
  data <- example_epikinetics_data()
  expect_error(
    prepare_epikinetics_data(data, lower_limit = 10, upper_limit = 5),
    "below its upper"
  )

  data <- example_epikinetics_data()
  data$censor <- "none"
  expect_error(
    prepare_epikinetics_data(data, lower_limit = 5, censoring = "censor"),
    "Uncensored observations must be greater"
  )

  data$censor <- "left"
  expect_error(
    prepare_epikinetics_data(data, lower_limit = 5, censoring = "censor"),
    "Left-censored observations must be at or below"
  )

  data$censor <- "right"
  expect_error(
    prepare_epikinetics_data(data, upper_limit = 2560, censoring = "censor"),
    "Right-censored observations must be at or above"
  )
})

test_that("invalid or redundant continuous covariates are rejected", {
  data <- example_epikinetics_data()
  data$age[data$pid == "P-03"] <- Inf
  expect_error(
    prepare_epikinetics_data(data, formula = ~ age),
    "only finite values"
  )

  data <- example_epikinetics_data()
  data$constant <- 1
  expect_error(
    prepare_epikinetics_data(data, formula = ~ constant),
    "must vary"
  )

  expect_error(
    prepare_epikinetics_data(data, formula = ~ age + I(age * 2)),
    "rank deficient"
  )
})
