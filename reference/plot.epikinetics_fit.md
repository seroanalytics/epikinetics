# Plot an epikinetics fit

Plot an epikinetics fit

## Usage

``` r
# S3 method for class 'epikinetics_fit'
plot(
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
  ...
)
```

## Arguments

- x:

  An `epikinetics_fit` object.

- type:

  Plot posterior population trajectories or prepared input data.

- newdata, times, ndraws, probs:

  Passed to
  [`predict.epikinetics_fit()`](https://seroanalytics.org/epikinetics/reference/predict.epikinetics_fit.md).

- scale:

  Prediction scale.

- central:

  Plot the posterior median or mean trajectory.

- biomarkers:

  Optional biomarker subset.

- show_data:

  Overlay observations for the conditional population plot.

- ...:

  Additional arguments reserved for plot methods.

## Value

A `ggplot` object.
