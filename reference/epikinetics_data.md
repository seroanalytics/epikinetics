# Extract prepared model data

Extract prepared model data

## Usage

``` r
epikinetics_data(x)
```

## Arguments

- x:

  An `epikinetics_data` or `epikinetics_fit` object.

## Value

The labelled, model-ready observation data frame. Use
[`stan_data()`](https://seroanalytics.org/epikinetics/reference/stan_data.md)
for the exact list passed to CmdStan.
