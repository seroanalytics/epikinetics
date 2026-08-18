# Fit a hierarchical biomarker kinetics model

Fits the epikinetics piecewise-linear model with CmdStanR and returns a
self-contained S3 fit object. The likelihood supports uncensored, left-
censored, and right-censored observations. Parallel chains and Stan
threads within each chain can be used together. Data validation and
transformation are deliberately performed beforehand by
[`prepare_epikinetics_data()`](https://seroanalytics.org/epikinetics/reference/prepare_epikinetics_data.md).

## Usage

``` r
fit_epikinetics(
  model_data,
  chains = 4,
  parallel_chains = chains,
  threads_per_chain = 1,
  grainsize = NULL,
  adapt_delta = 0.9,
  max_treedepth = 12,
  ...
)
```

## Arguments

- model_data:

  An object returned by
  [`prepare_epikinetics_data()`](https://seroanalytics.org/epikinetics/reference/prepare_epikinetics_data.md).

- chains:

  Number of Markov chains.

- parallel_chains:

  Number of chains to run concurrently.

- threads_per_chain:

  Number of Stan threads used by each chain. The model is always
  compiled with threading enabled.

- grainsize:

  Number of participants handled by each `reduce_sum` task. By default
  it is chosen from the participant and thread counts.

- adapt_delta:

  Target acceptance probability. The conservative default is appropriate
  for this nonlinear hierarchy and may be overridden after inspecting
  diagnostics.

- max_treedepth:

  Maximum NUTS tree depth.

- ...:

  Additional arguments passed to the CmdStanR model's `$sample()`
  method, such as `iter_warmup`, `iter_sampling`, and `seed`.

## Value

An object of class `epikinetics_fit`.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- utils::read.csv(
  system.file("extdata", "delta.csv", package = "epikinetics")
)
model_data <- prepare_epikinetics_data(
  dat,
  formula = ~ infection_history,
  lower_limit = 5,
  upper_limit = 2560
)
model_data
model.matrix(model_data)
stan_data(model_data)

fit <- fit_epikinetics(
  model_data,
  chains = 4,
  parallel_chains = 4,
  threads_per_chain = 2
)
} # }
```
