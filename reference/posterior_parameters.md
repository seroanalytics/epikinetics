# Extract labelled kinetic parameters

Extract labelled kinetic parameters

## Usage

``` r
posterior_parameters(
  x,
  level = c("population", "profile", "participant", "regression"),
  summary = TRUE,
  newdata = NULL,
  participants = NULL,
  ndraws = NULL,
  probs = c(0.025, 0.975),
  derived = TRUE
)
```

## Arguments

- x:

  An `epikinetics_fit` object.

- level:

  One of `"population"`, `"profile"`, `"participant"`, or
  `"regression"`. Population output is the biomarker-specific model
  intercept. Profile output applies the fitted covariate effects to the
  conditional profiles from
  [`prediction_grid()`](https://seroanalytics.org/epikinetics/reference/prediction_grid.md)
  or `newdata`, but excludes participant random effects.

- summary:

  If `TRUE`, return means, medians, SDs, and intervals. If `FALSE`,
  return one labelled row per posterior draw.

- newdata:

  Optional participant-level covariate profiles when
  `level = "profile"`. `NULL` uses
  [`prediction_grid()`](https://seroanalytics.org/epikinetics/reference/prediction_grid.md).

- participants:

  Optional participant ids when `level = "participant"`.

- ndraws:

  Optional maximum number of posterior draws to use.

- probs:

  Lower and upper interval probabilities.

- derived:

  Include peak values, transition values, and waning half-lives.

## Value

A tidy data frame. Summaries use one row per parameter and group;
unsummarised results retain parameter columns and `.draw`. Regression
output includes the original formula term, design-column name, and
decoded treatment level/reference when the contrast has that
interpretation.
