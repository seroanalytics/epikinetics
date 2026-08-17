# Developing epikinetics

## Set up a fresh checkout

CmdStanR is distributed through the Stan R-universe rather than CRAN. Configure
that repository in a fresh R session before pak or devtools resolves the local
package dependencies:

```r
install.packages("pak")
pak::repo_add(stan = "https://stan-dev.r-universe.dev")
pak::local_install_dev_deps(
  ".",
  dependencies = c("hard", "soft", "Config/Needs/website")
)
```

`pak::repo_add()` modifies `getOption("repos")` for the current R session. To
make the setting persistent, add this to a user or project `.Rprofile`:

```r
options(repos = c(
  stan = "https://stan-dev.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
```

The package's `Additional_repositories` field is retained because it is the
correct package metadata for the location of CmdStanR. It is not, however, a
replacement for configuring the active repositories used by every dependency
solver.

Installing the CmdStanR R package is sufficient for building the package and
its vignettes. The separate CmdStan toolchain is required only for compiling
and fitting the Stan model or running the opt-in Stan integration tests:

```r
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()
```

## Tests and documentation

Run the fast tests and the standard package check with:

```r
devtools::test()
devtools::check(args = "--no-manual")
```

Preview one or all vignette articles with:

```r
pkgdown::build_article("getting-started")
pkgdown::build_articles()
```

Rendering R Markdown vignettes also requires Pandoc. RStudio and Quarto
normally provide it; confirm that the current R session can find it with
`rmarkdown::pandoc_available()` if rendering fails after dependencies have
been installed.

Use `devtools::build()` to exercise the real source-package build, including
vignettes. `devtools::build_vignettes()` is deprecated in devtools 2.5.0 and
later.

The lightweight Stan integration test is deliberately opt-in:

```r
Sys.setenv(EPIKINETICS_RUN_STAN_TESTS = "true")
testthat::test_local(".", filter = "stan", load_package = "source")
```
