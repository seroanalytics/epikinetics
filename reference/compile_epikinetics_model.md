# Compile the threaded epikinetics Stan model

Compilation is deliberately explicit and cached in R's per-user cache
directory. Package installation and loading never install CmdStan or
compile C++.
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
calls this function automatically when needed.

## Usage

``` r
compile_epikinetics_model(
  force_recompile = FALSE,
  quiet = TRUE,
  cache_dir = getOption("epikinetics.cache_dir", tools::R_user_dir("epikinetics", which =
    "cache"))
)
```

## Arguments

- force_recompile:

  Recompile even when the current source has a cached executable.

- quiet:

  Passed to
  [`cmdstanr::cmdstan_model()`](https://mc-stan.org/cmdstanr/reference/cmdstan_model.html).

- cache_dir:

  Optional cache directory. The default is controlled by the
  `epikinetics.cache_dir` option and otherwise uses
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html).

## Value

A `CmdStanModel` compiled with Stan threading enabled.
