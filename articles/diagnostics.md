# Diagnostics and posterior prediction

## Inspect inputs before fitting

Prepared-data plots are independent of the sampler. They return ordinary
`ggplot` objects and can be customised with ggplot2. Prior specification
and prior trajectory checks are covered in the
[Priors](https://seroanalytics.org/epikinetics/articles/priors.md)
vignette.

``` r

library(epikinetics)
dat <- read.csv(
  system.file("extdata", "delta.csv", package = "epikinetics")
)
prepared <- prepare_epikinetics_data(
  dat,
  formula = ~ infection_history,
  lower_limit = 5,
  upper_limit = 2560
)

summary(prepared)
#> Prepared epikinetics model-data summary
#> observations participants   biomarkers   covariates 
#>         2255          335            3            1 
#> 
#> Ranges
#>             quantity  minimum    maximum
#>  time_since_exposure 0.000000  578.00000
#>             response 5.000000 2560.00000
#>       model_response 2.321928   11.32193
#> 
#> Censoring
#>  censoring observations
#>       none         2003
#>       left          126
#>      right          126
#> 
#> Participant observation counts
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   3.000   5.000   6.000   6.731   9.000  14.000 
#> 
#> Formula: ~infection_history
#> Transformation: log2(value / 1)
#> Model-matrix columns: infection_historyPreviously infected (Pre-Omicron)
#> Formula affects: baseline, time_to_peak, waning_duration, boost_rate, early_waning_rate, late_waning_rate
#> Participant random effects: baseline, boost_rate, early_waning_rate, late_waning_rate
#> Biomarker order (first appearance): Ancestral, Alpha, Delta
#> Factor reference levels: infection_history=Infection naive
#> 
#> Design-column mapping
#>                                       design_column              term
#>  infection_historyPreviously infected (Pre-Omicron) infection_history
#>          variables                             level reference_level
#>  infection_history Previously infected (Pre-Omicron) Infection naive
#>                                                                     label
#>  infection_history=Previously infected (Pre-Omicron) (vs Infection naive)
model.matrix(prepared)[1:5, , drop = FALSE]
#>   infection_historyPreviously infected (Pre-Omicron)
#> 1                                                  0
#> 2                                                  0
#> 3                                                  0
#> 4                                                  1
#> 5                                                  0
prepared$mappings$participant[1:5, ]
#>   participant_index participant observation_count
#> 1                 1           1                 9
#> 2                 2           2                12
#> 3                 3           3                 6
#> 4                 4           4                 9
#> 5                 5           5                12
table(prepared$observations$censoring)
#> 
#>  left  none right 
#>   126  2003   126
names(stan_data(prepared))
#>  [1] "N_observations"             "N_participants"            
#>  [3] "N_biomarkers"               "N_covariates"              
#>  [5] "biomarker"                  "time"                      
#>  [7] "value"                      "censoring"                 
#>  [9] "lower_limit"                "upper_limit"               
#> [11] "observation_start"          "observation_end"           
#> [13] "participant_sequence"       "X"                         
#> [15] "covariate_active"           "participant_effect_active" 
#> [17] "grainsize"                  "population_prior_mean"     
#> [19] "population_prior_sd"        "participant_sd_prior_scale"
#> [21] "covariate_prior_scale"      "observation_sd_prior_scale"

plot(prepared)
```

![Prepared observations on the response scale. Point shapes distinguish
uncensored and censored measurements; dashed lines mark assay
limits.](diagnostics_files/figure-html/before-fitting-1.png)

Prepared observations on the response scale. Point shapes distinguish
uncensored and censored measurements; dashed lines mark assay limits.

## Sampling diagnostics

The following examples assume a fit created as in the getting-started
vignette. They are not evaluated when the vignette is built.

``` r

diagnostics <- diagnose_epikinetics(fit)
diagnostics$overview
diagnostics$sampler
diagnostics$chains
diagnostics$parameters
```

The overview reports:

- divergent transitions;
- transitions hitting maximum tree depth;
- chains with non-finite E-BFMI, separately from chains below 0.3;
- maximum R-hat among principal population parameters; and
- minimum bulk and tail effective sample sizes for those parameters.

These are screening summaries. Inspect parameter-level diagnostics and
trace plots whenever the screening result is poor. The full CmdStanR
object is available without copying draws:

``` r

stan_fit <- cmdstan_fit(fit)
stan_fit$diagnostic_summary()
stan_fit$cmdstan_diagnose()
posterior::as_draws_df(stan_fit$draws())
```

`diagnostics$chains` also reports the number and variance of retained
energy values. For example, `E-BFMI = NaN` together with one unique
energy value means the chain was frozen: both the numerator and
denominator of the E-BFMI ratio are zero. It is not a benign missing
diagnostic. Predictions keep all retained chains, so they warn when
E-BFMI is non-finite instead of silently selecting the apparently better
chains.

If all or some chains fail,
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
preserves the CmdStanR object and marks the returned object as failed or
partial whenever CmdStanR supplies a run object. The original chain
output remains the primary debugging record:

``` r

fit$computation$sampling_state
cmdstan_fit(fit)$return_codes()
cmdstan_fit(fit)$output()
```

This complements inspection of `stan_data(prepared)`; it does not wrap
or replace CmdStanR’s diagnostic interface.

The previous package’s Shiny inspector explored inputs and priors but
was not a posterior diagnostic application. It has been removed from the
core interface: the lighter plotting functions cover its maintainable
role, and CmdStanR plus the posterior ecosystem provide more complete
sampler diagnostics.

## Latent trajectories versus observations

By default, [`predict()`](https://rdrr.io/r/stats/predict.html) returns
the latent expected biomarker curve, excluding residual measurement
noise. Set `include_observation_noise = TRUE` when the target is a
future observed measurement rather than its latent mean.

``` r

latent <- predict(
  fit,
  times = 0:150,
  type = "population",
  summary = FALSE,
  ndraws = 500
)

observed <- predict(
  fit,
  times = 0:150,
  type = "population",
  include_observation_noise = TRUE,
  summary = FALSE,
  ndraws = 500,
  seed = 10
)
```

Keeping `summary = FALSE` preserves the `.draw` identifier, which should
be retained when combining trajectory quantities across times or
biomarkers. With `summary = TRUE`, `lower` and `upper` are calculated
across these raw draw-specific trajectories at each
biomarker/time/profile combination. The plot ribbon uses those columns
directly.

## Prediction targets

Population predictions omit participant random effects. When `newdata`
is `NULL`,
[`prediction_grid()`](https://seroanalytics.org/epikinetics/reference/prediction_grid.md)
supplies the combinations of categorical values observed among fitted
participants and fixes continuous predictors at their participant-level
medians. This avoids silent extrapolation to impossible factor
combinations. It is a conditional estimand, not marginalisation over the
sample’s covariate distribution.

``` r

prediction_grid(fit)
population <- predict(fit, type = "population")
plot(population)

# Explicit newdata selects or overrides the conditional profiles.
profiles <- data.frame(
  infection_history = c(
    "Infection naive",
    "Previously infected (Pre-Omicron)"
  )
)
population <- predict(fit, newdata = profiles, type = "population")
plot(population)
```

For multiple categorical predictors the default includes only observed
combinations. `prediction_grid(fit, categorical = "cartesian")`
deliberately requests every fitted-level combination. The stored R terms
object and contrasts evaluate main effects, interactions, and
transformed terms for both automatic and explicit grids. Unknown factor
levels fail before posterior prediction with an error naming the
variable, supplied level, and fitted levels.

Existing-participant predictions combine population, covariate, and
fitted random effects:

``` r

individual <- predict(
  fit,
  type = "individual",
  participants = c("1", "2"),
  times = 0:180
)
plot(individual)
```

New-participant predictions draw one new standardised random effect per
*selected* participant-level kinetic quantity and posterior draw.
Inactive quantities remain at their conditional population/profile
values. Each active effect is used across biomarkers, in accordance with
the fitted hierarchy:

``` r

new_individuals <- predict(
  fit,
  newdata = profiles,
  type = "new",
  times = 0:180,
  seed = 34
)
plot(new_individuals)
```

These new-participant results represent one hypothetical participant per
profile per draw. They are not an average over a newly simulated cohort.

## Scales and plots

Response-scale output is the default and applies the reference stored
during data preparation. Use `scale = "model"` for log2 output.
`plot(fit)` produces formula-aware conditional population trajectories
with observations overlaid, while `plot(fit, type = "data")` shows
prepared inputs only.

``` r

plot(fit)
plot(fit, type = "data")

p <- plot(predict(fit, type = "population", times = 0:180))
p + ggplot2::theme_minimal()
```

Plot methods do no scientific aggregation beyond the documented
posterior summary. For custom analyses, work with the returned
prediction data frame or raw draws directly.
