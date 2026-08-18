# Plot fitted individual trajectories

Generates (or reuses) fitted-participant latent trajectory summaries and
combines them with observations, censoring indicators, and assay limits.

## Usage

``` r
plot_individual(
  x,
  participant,
  times = 0:150,
  ndraws = 500,
  probs = c(0.025, 0.975),
  scale = c("response", "model"),
  biomarkers = NULL,
  central = c("median", "mean"),
  ...
)
```

## Arguments

- x:

  An `epikinetics_fit` or individual `epikinetics_prediction` object.

- participant:

  One fitted participant id.

- times, ndraws, probs, scale, biomarkers:

  Passed to [`predict()`](https://rdrr.io/r/stats/predict.html). Ignored
  when `x` is already a prediction object.

- central:

  Plot the posterior median or mean trajectory.

- ...:

  Reserved for future methods.

## Value

An ordinary `ggplot` object.
