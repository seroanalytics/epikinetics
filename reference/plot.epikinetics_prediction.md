# Plot posterior trajectory predictions

Uses the labelled output of
[`predict.epikinetics_fit()`](https://seroanalytics.org/epikinetics/reference/predict.epikinetics_fit.md)
directly. Biomarkers are overlaid using colour and fill; categorical
covariate profiles determine facets. Multiple categorical variables are
combined into readable labels. Individual predictions use one panel per
participant.

## Usage

``` r
# S3 method for class 'epikinetics_prediction'
plot(x, central = c("median", "mean"), show_observations = TRUE, ...)
```

## Arguments

- x:

  An `epikinetics_prediction` data frame.

- central:

  Plot the posterior `"median"` (default) or `"mean"` as the central
  trajectory. Both are always retained in summarised predictions.

- show_observations:

  Show stored observations for individual predictions.

- ...:

  Reserved for future methods.

## Value

An ordinary `ggplot` object.
