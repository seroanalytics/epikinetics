# Validate and prepare data for epikinetics

Converts an ordinary data frame into the complete, participant-indexed
representation used by the Stan model. This is a separate public
operation from
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
so that transformations, censoring, indices, mappings, and the design
matrix can be inspected before compilation or sampling.

## Usage

``` r
prepare_epikinetics_data(
  data,
  formula = ~1,
  covariate_parameters = "all",
  participant_parameters = c("baseline", "boost_rate", "early_waning_rate",
    "late_waning_rate"),
  id = "pid",
  time = "day",
  exposure = "last_exp_day",
  biomarker = "titre_type",
  value = "value",
  biomarker_order = NULL,
  scale = c("natural", "log2"),
  reference_value = 1,
  lower_limit = NULL,
  upper_limit = NULL,
  censoring = NULL,
  priors = epikinetics_priors()
)
```

## Arguments

- data:

  A `data.frame` or object inheriting from it.

- formula:

  One-sided R formula for participant-level covariates. Numeric
  variables remain on their supplied scale; factors use the active R
  contrasts. Use `~ 1` for no covariates.

- covariate_parameters:

  Kinetic parameters modified by the formula. The default, `"all"`,
  applies the same design matrix to all six parameters, matching the
  original model. Supply any subset of `baseline`, `time_to_peak`,
  `waning_duration`, `boost_rate`, `early_waning_rate`, and
  `late_waning_rate` for a more explicit regression specification.

- participant_parameters:

  Kinetic parameters with participant-level random effects. The default
  allows participants to differ in `baseline`, `boost_rate`,
  `early_waning_rate`, and `late_waning_rate`, while sharing
  `time_to_peak` and `waning_duration` after conditioning on any
  selected covariate effects. In other words, those timing parameters
  have no residual participant random effect by default. This is a
  modelling assumption suited to the package's motivating data, not a
  universal biological constraint. Supply any parameter subset, or
  `"all"`, for another justified hierarchy.

- id, time, exposure, biomarker, value:

  Column names identifying participant, observation time, focal exposure
  time, biomarker type, and measurement. Set `exposure = NULL` when
  `time` is already numeric time since exposure, with the exposure at
  zero (for example after
  [`align_time_to_reference()`](https://seroanalytics.org/epikinetics/reference/align_time_to_reference.md)).

- biomarker_order:

  Optional complete ordering of observed biomarker labels. If omitted,
  existing factor levels are preserved; otherwise the order of first
  appearance in `data` is used. The order is stored in the prepared
  object and propagated to posterior output, predictions, and plots.

- scale:

  Either `"natural"` (positive values transformed with base-2
  logarithms) or `"log2"` (values already on the model scale).

- reference_value:

  Positive reference used for natural-scale data:
  `log2(value / reference_value)`.

- lower_limit, upper_limit:

  Optional censoring limits. Each may be a scalar, a numeric vector with
  one value per row, a named numeric vector by biomarker, or the name of
  a numeric column. Use `NA` for no limit on a row.

- censoring:

  Optional censoring indicator or column name. Accepted values are
  `"none"`, `"left"`, and `"right"` (or `0`, `-1`, and `1`). When
  omitted, measurements at or beyond supplied limits are classified
  automatically.

- priors:

  An
  [`epikinetics_priors()`](https://seroanalytics.org/epikinetics/reference/epikinetics_priors.md)
  object.

## Value

An `epikinetics_data` object. Its public components include
`input_data`, `observations`, `participants`, `model_frame`,
`model_matrix`, `mappings`, and `stan_data`. `mappings$design_columns`
links numeric design columns back to terms and treatment levels. Use
[`stan_data()`](https://seroanalytics.org/epikinetics/reference/stan_data.md)
and [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) as
accessors.

## Details

The model assumes one focal exposure per participant. Covariates in
`formula` must likewise be constant within participant. Standard R
contrast handling is used: include the formula intercept (for example
`~ age + sex`), and epikinetics removes that intercept because
biomarker-specific population parameters already provide it.
