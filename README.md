# epikinetics
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![R-CMD-check](https://github.com/seroanalytics/epikinetics/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/seroanalytics/epikinetics/actions/workflows/check-standard.yaml) [![codecov](https://codecov.io/gh/seroanalytics/epikinetics/graph/badge.svg?token=5MZYYDUZYH)](https://codecov.io/gh/seroanalytics/epikinetics)

`epikinetics` is an R package for Bayesian hierarchical modelling of antibody kinetics.

The underlying model is taken from [Russell TW et al., Real-time estimation of immunological responses against emerging SARS-CoV-2 variants in the UK: a mathematical modelling study.](#References)
See the [case study vignette](https://seroanalytics.org/epikinetics/articles/biokinetics.html) for a replication of some key figures from the paper using `epikinetics`.

Three publicly available datasets from the above paper are also installed with the package and 
can be used as test input data:

```{r}
delta <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))
ba2 <- data.table::fread(system.file("ba2_full.rds", package = "epikinetics"))
xbb <- data.table::fread(system.file("xbb_full.rds", package = "epikinetics"))
```

If running the model with your own data, see the [data vignette](https://seroanalytics.org/epikinetics/articles/data.html) for 
an explanation of the input format. 

# Installing

To interface with [cmdstan](https://mc-stan.org/users/interfaces/cmdstan), this package uses `cmdstanr`, which isn't available on cran, so you will first have to install it as follows:

```
install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))
```

You can then install `epikinetics` from GitHub:

```
remotes::install_github("seroanalytics/epikinetics")
```

## Troubleshooting installation

If you don't already have [cmdstan](https://mc-stan.org/users/interfaces/cmdstan) installed, the `epikinetics` installer will attempt 
to install it, which can take a few minutes. If you see errors as part of installation, it is 
probably a good idea to try and install `cmdstan` first, for easier debugging. You can 
do this using the `cmdstanr` package as follows:

```{r}
cmdstanr::install_cmdstan()
```

Verify the installation is working with

```{r}
cmdstanr::cmdstan_version()
```

# Running in Docker

Alternatively, you can run `epikinetics` via a Docker image, mounting a working directory which contains your input data files:

```
docker pull seroanalytics/epikinetics:main
docker run -v /path/to/local/workdir:/workdir -it seroanalytics/epikinetics:main
```

# Developing

This package relies on the [instantiate](https://wlandau.github.io/instantiate/) package 
to ship pre-compiled stan models. See the `src/install.libs.R` file for the logic for compiling 
and installing the stan models during package installation. 

When running via `devtools` (e.g. `test` or `load_all`) the `install.libs.R` logic is not run, 
so for this we use an `onLoad` hook which checks whether the package is being loaded via `devtools`
and if so, copies compiled models into a local `bin` directory where the `system.file` shim
can access them.

Note that if you are running `devtools::load_all` or `devtool::test` and you don't yet 
have `cmdstan` installed, this will trigger an installation of `cmdstan` which can take 
a few minutes.

## Testing

To run all tests locally:

```{r}
devtools::test()
```

To run tests in a single file:

```{r}
devtools::test(filter="filename")
```

Some tests are skipped on CI to avoid exorbitantly long build times, but this means 
it is important to run all tests locally at least once before merging a pull request.

For snapshot testing of stan model outputs, we need the outputs to be exactly 
reproducible. As well as setting a seed, this requires the machine environment 
to be exactly the same, so on CI we run these inside a Docker container, via a bash script:

```{shell}
./tests/snapshots/test-snapshots
```

This involves recompiling the model, so takes a while to run.

Note that 

## Docker
To build a Docker image, run `docker/build`. 
To push a new image to Dockerhub, `docker/push`. An image is built and pushed 
during CI on merge to main.

# References
Russell TW, Townsley H, Hellewell J, Gahir J, Shawe-Taylor M, Greenwood D, Hodgson D, Hobbs A, Dowgier G, Penn R, Sanderson T, Stevenson-Leggett P, Bazire J, Harvey R, Fowler AS, Miah M, Smith C, Miranda M, Bawumia P, Mears HV, Adams L, Hatipoglu E, O'Reilly N, Warchal S, Ambrose K, Strange A, Kelly G, Kjar S, Papineni P, Corrah T, Gilson R, Libri V, Kassiotis G, Gamblin S, Lewis NS, Williams B, Swanton C, Gandhi S, Beale R, Wu MY, Bauer DLV, Carr EJ, Wall EC, Kucharski AJ. Real-time estimation of immunological responses against emerging SARS-CoV-2 variants in the UK: a mathematical modelling study. Lancet Infect Dis. 2024 Sep 11:S1473-3099(24)00484-5. doi: [10.1016/S1473-3099(24)00484-5](https://doi.org/10.1016/s1473-3099(24)00484-5).
