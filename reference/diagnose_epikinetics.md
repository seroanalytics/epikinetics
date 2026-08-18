# Summarise sampling diagnostics

Summarise sampling diagnostics

## Usage

``` r
diagnose_epikinetics(x, run_cmdstan = FALSE, quiet = FALSE)
```

## Arguments

- x:

  An `epikinetics_fit` object.

- run_cmdstan:

  If `TRUE`, also run CmdStan's text diagnostic utility.

- quiet:

  Suppress the compact console report.

## Value

Invisibly, a list containing sampler diagnostics and summaries for the
principal population parameters.
