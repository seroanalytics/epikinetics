# Construct default covariate profiles for prediction

Builds inspectable participant-level profiles from the formula and model
frame stored by
[`prepare_epikinetics_data()`](https://seroanalytics.org/epikinetics/reference/prepare_epikinetics_data.md).
By default, categorical profiles are the combinations observed among
fitted participants; this avoids silently extrapolating to unsupported
combinations. Continuous covariates are held at their participant-level
median. Use `categorical = "cartesian"` to request every combination of
fitted factor levels, or pass an explicit `newdata` data frame to
[`predict()`](https://rdrr.io/r/stats/predict.html).

## Usage

``` r
prediction_grid(
  x,
  categorical = c("observed", "cartesian"),
  continuous = c("median", "mean")
)
```

## Arguments

- x:

  An `epikinetics_data` or `epikinetics_fit` object.

- categorical:

  Use combinations `"observed"` in the participant data or the full
  `"cartesian"` product of fitted categorical levels.

- continuous:

  Hold continuous covariates at their participant-level `"median"`
  (default) or `"mean"`.

## Value

An ordinary data frame with one row per prediction profile. The
`.profile` column is a stable row identifier; remaining columns are the
original variables used by the model formula.

## Details

These are conditional profiles, not averages over the fitted covariate
distribution. Interactions and transformed terms are subsequently
evaluated with the original terms object and contrasts when
[`predict()`](https://rdrr.io/r/stats/predict.html) constructs the
numeric design matrix.

## Examples

``` r
dat <- utils::read.csv(
  system.file("extdata", "delta.csv", package = "epikinetics")
)
prepared <- prepare_epikinetics_data(
  dat,
  formula = ~ infection_history,
  lower_limit = 5,
  upper_limit = 2560
)
prediction_grid(prepared)
#>   .profile                 infection_history
#> 1        1                   Infection naive
#> 2        2 Previously infected (Pre-Omicron)
```
