# Plot prior kinetic trajectories

Draw population-level kinetic curves from the configured priors. The
band shows the pointwise 95% prior interval and the line shows the
pointwise prior median.

Construct and inspect the priors used by
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md).
Population priors are Normal distributions parameterised by
`c(mean, sd)`. Positive kinetic quantities use Normal priors truncated
at zero. Their prior means must be non-negative so that the inverse-CDF
parameterisation remains numerically stable.

## Usage

``` r
# S3 method for class 'epikinetics_priors'
plot(
  x,
  ...,
  times = 0:150,
  ndraws = 1000,
  probs = c(0.025, 0.975),
  reference_value = 1,
  scale = c("model", "response")
)

epikinetics_priors(
  baseline = c(mean = 6, sd = 2),
  time_to_peak = c(mean = 10, sd = 5),
  waning_duration = c(mean = 50, sd = 20),
  boost_rate = c(mean = 0.25, sd = 0.1),
  early_waning_rate = c(mean = 0.02, sd = 0.01),
  late_waning_rate = c(mean = 0.002, sd = 0.005),
  participant_sd = c(baseline = 0.75, time_to_peak = 0.35, waning_duration = 0.5,
    boost_rate = 0.35, early_waning_rate = 0.5, late_waning_rate = 0.75),
  covariate_sd = c(baseline = 0.5, time_to_peak = 0.35, waning_duration = 0.5, boost_rate
    = 0.35, early_waning_rate = 0.5, late_waning_rate = 0.75),
  observation_sd = 1
)
```

## Arguments

- x:

  An `epikinetics_priors` object.

- ...:

  Reserved for compatibility with the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic.

- times:

  Non-negative prediction times.

- ndraws:

  Number of prior trajectories to simulate.

- probs:

  Lower and upper probabilities for the pointwise prior interval.

- reference_value:

  Positive reference value used to convert model-scale values to the
  response scale when `scale = "response"`.

- scale:

  Output scale: `"model"` for log2-relative values or `"response"` for
  natural measurement values.

- baseline, time_to_peak, waning_duration, boost_rate,
  early_waning_rate, late_waning_rate:

  Numeric length-two vectors giving the population prior mean and
  standard deviation.

- participant_sd:

  Named positive vector giving half-Normal scales for participant-level
  standard deviations.

- covariate_sd:

  Named positive vector giving Normal standard deviations for regression
  coefficients.

- observation_sd:

  Positive half-Normal scale for observation error.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

An object of class `epikinetics_priors`.

## Details

Baseline is expressed on the model's log2 scale. Time parameters are in
the same units as the input time columns (normally days). Rates are log2
units per time unit. Participant standard deviations act additively for
baseline and on the log scale for positive time/rate parameters.
Covariate priors use the same convention.

## Examples

``` r
priors <- epikinetics_priors(
  time_to_peak = c(mean = 12, sd = 4),
  early_waning_rate = c(mean = 0.025, sd = 0.01)
)
priors
#>          parameter population_mean population_sd participant_sd_scale
#>           baseline           6.000         2e+00                 0.75
#>       time_to_peak          12.000         4e+00                 0.35
#>    waning_duration          50.000         2e+01                 0.50
#>         boost_rate           0.250         1e-01                 0.35
#>  early_waning_rate           0.025         1e-02                 0.50
#>   late_waning_rate           0.002         5e-03                 0.75
#>  covariate_sd
#>          0.50
#>          0.35
#>          0.50
#>          0.35
#>          0.50
#>          0.75
#> 
#> Observation SD half-Normal scale: 1 
```
