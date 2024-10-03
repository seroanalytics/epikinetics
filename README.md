# Epikinetics
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![R-CMD-check](https://github.com/seroanalytics/epikinetics/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/seroanalytics/epikinetics/actions/workflows/check-standard.yaml) [![codecov](https://codecov.io/gh/seroanalytics/epikinetics/graph/badge.svg?token=5MZYYDUZYH)](https://codecov.io/gh/seroanalytics/epikinetics)

# Installing

This package uses `cmdstanr`, which isn't available on cran, so you will first have to install it as follows:

```
install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))
```

You can then install `epikinetics` from GitHub:

```
remotes::install_github("seroanalytics/epikinetics")
```

# Running in Docker

Alternatively, you can run `epikinetics` via a Docker image, mounting a working directory which contains your input data files:

```
docker run -v /workdir:/ -it seroanalytics/epikinetics:main
```

# Developing

This package relies on the [instantiate](https://wlandau.github.io/instantiate/) package 
to ship pre-compiled stan models. See the `src/install.libs.R` file for the logic.

This does seem to mean that `devtools::load_all()` won't work, which is a bit annoying. For testing
local changes you have to actually run `devtools::install()`.

## Docker
To build a Docker image, run `docker/build`. 
To push a new image to Dockerhub, `docker/push`. An image is built and pushed 
during CI on merge to main.