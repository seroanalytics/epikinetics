# Access the exact Stan data list

Returns the complete, sampling-ready list. For prepared data its default
`grainsize` may be adjusted by
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
for the requested thread count. For a fit, the result includes the final
grainsize used by that run.

## Usage

``` r
stan_data(x)
```

## Arguments

- x:

  An `epikinetics_data` or `epikinetics_fit` object.

## Value

A named list suitable for a CmdStanR model's `$sample(data = ...)`
argument.
