test_that("raw draws and population parameters remain accessible", {
  fit <- fake_epikinetics_fit()
  draws <- posterior_draws(fit)
  expect_true(posterior::is_draws(draws))

  population <- posterior_parameters(fit, summary = FALSE, ndraws = 5)
  expect_length(unique(population$.draw), 5)
  expect_equal(range(population$.draw), c(1L, 20L))
  expect_setequal(unique(population$biomarker), c("A", "B"))
  expect_true(all(c(
    "baseline", "time_to_peak", "waning_change_time", "peak_response",
    "early_waning_half_life"
  ) %in% names(population)))

  summary <- posterior_parameters(fit)
  expect_true(all(c("parameter", "median", "lower", "upper") %in% names(summary)))
})

test_that("posterior parameter output preserves biomarker order", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group,
    biomarker_order = c("B", "A")
  )
  fit <- fake_epikinetics_fit(prepared)
  population <- posterior_parameters(fit, summary = FALSE, ndraws = 3)
  participant <- posterior_parameters(
    fit,
    level = "participant",
    participants = "P-01",
    summary = FALSE,
    ndraws = 3
  )
  expect_equal(levels(population$biomarker), c("B", "A"))
  expect_equal(levels(participant$biomarker), c("B", "A"))
})

test_that("participant parameters are labelled and constrained coherently", {
  fit <- fake_epikinetics_fit()
  participant <- posterior_parameters(
    fit,
    level = "participant",
    participants = "P-02",
    summary = FALSE,
    ndraws = 4
  )
  expect_equal(unique(participant$participant), "P-02")
  expect_equal(as.character(unique(participant$group)), "treated")
  expect_true(all(participant$time_to_peak > 0))
  expect_true(all(participant$waning_duration > 0))
  expect_true(all(participant$boost_rate > 0))
  expect_true(all(participant$early_waning_rate > 0))
  expect_true(all(participant$late_waning_rate > 0))
  expect_true(all(participant$waning_change_time > participant$time_to_peak))
  expect_error(
    posterior_parameters(fit, level = "participant", participants = "missing"),
    "Unknown participant"
  )
})

test_that("conditional profile parameters retain formula and biomarker labels", {
  fit <- fake_epikinetics_fit()
  profiles <- posterior_parameters(
    fit,
    level = "profile",
    summary = FALSE,
    ndraws = 3
  )
  expect_setequal(as.character(unique(profiles$group)),
                  c("reference", "treated"))
  expect_setequal(unique(profiles$biomarker), c("A", "B"))
  expect_true(all(c("late_waning_half_life", "peak_response") %in%
                    names(profiles)))

  summary <- posterior_parameters(fit, level = "profile", ndraws = 3)
  expect_true(all(c(
    ".profile", "biomarker", "group", "parameter", "median", "lower", "upper"
  ) %in% names(summary)))
  expect_error(
    posterior_parameters(fit, level = "population", newdata = data.frame()),
    "only used when level = 'profile'"
  )
})

test_that("regression effects have interpretable multiplicative forms", {
  fit <- fake_epikinetics_fit()
  regression <- posterior_parameters(fit, level = "regression", summary = FALSE)
  expect_setequal(unique(regression$parameter), c(
    "baseline", "time_to_peak", "waning_duration", "boost_rate",
    "early_waning_rate", "late_waning_rate"
  ))
  expect_equal(
    regression$multiplicative_effect[regression$parameter == "baseline"],
    2^regression$coefficient[regression$parameter == "baseline"]
  )
  expect_equal(unique(regression$term), "group")
  expect_equal(unique(regression$design_column), "grouptreated")
  expect_equal(unique(regression$level), "treated")
  expect_equal(unique(regression$reference_level), "reference")
  expect_equal(
    unique(regression$covariate),
    "group=treated (vs reference)"
  )

  summary <- posterior_parameters(fit, level = "regression")
  expect_true(all(c("parameter", "quantity", "median") %in% names(summary)))
})

test_that("regression output contains only selected kinetic parameters", {
  prepared <- prepare_epikinetics_data(
    example_epikinetics_data(),
    formula = ~ group,
    covariate_parameters = c("baseline", "late_waning_rate")
  )
  fit <- fake_epikinetics_fit(prepared)
  regression <- posterior_parameters(
    fit,
    level = "regression",
    summary = FALSE
  )
  expect_setequal(
    unique(regression$parameter),
    c("baseline", "late_waning_rate")
  )
})
