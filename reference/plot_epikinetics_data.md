# Plot prepared epikinetics observations

Plot prepared epikinetics observations

## Usage

``` r
plot_epikinetics_data(x, scale = c("response", "model"), ...)
```

## Arguments

- x:

  An `epikinetics_data` or `epikinetics_fit` object.

- scale:

  Plot values on the response or model log2 scale. Response-scale axes
  use log2 spacing, retaining natural-scale labels while respecting the
  multiplicative nature of titre measurements.

- ...:

  Reserved for future methods.

## Value

A `ggplot` object.
