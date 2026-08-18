# Data, covariates, scales, and censoring

## Required observation data

[`prepare_epikinetics_data()`](https://seroanalytics.org/epikinetics/reference/prepare_epikinetics_data.md)
accepts ordinary data frames, including `data.table` and tibble
subclasses.
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
then accepts the prepared object, keeping validation and transformation
separate from compilation and sampling. Defaults expect:

| Column | Meaning |
|----|----|
| `pid` | Participant identifier; numeric or character values are accepted. |
| `day` | Observation time. |
| `last_exp_day` | Time of the focal exposure. |
| `titre_type` | Biomarker or assay label. |
| `value` | Finite positive biomarker value on the natural scale. |

Use `id`, `time`, `exposure`, `biomarker`, and `value` arguments for
other names. Observation and exposure times may both be numeric, `Date`,
`POSIXt`, or parseable ISO date strings. The difference defines time
since exposure. Times before exposure are rejected.

``` r

library(epikinetics)
dat <- read.csv(
  system.file("extdata", "delta.csv", package = "epikinetics")
)
dat[1:6, c("pid", "day", "last_exp_day", "titre_type", "value")]
#>   pid        day last_exp_day titre_type    value
#> 1   1 2021-03-10   2021-03-08  Ancestral 175.9350
#> 2   1 2021-04-15   2021-03-08  Ancestral 607.5750
#> 3   1 2021-07-08   2021-03-08  Ancestral 179.0463
#> 4   1 2021-03-10   2021-03-08      Alpha   5.0000
#> 5   1 2021-04-15   2021-03-08      Alpha 416.7905
#> 6   1 2021-07-08   2021-03-08      Alpha 103.5274
```

The model currently represents one known focal exposure per participant,
so `last_exp_day` must be constant within participant. Missing required
values and missing covariates are rejected early.

## Scale

The Gaussian observation model operates on a base-2 logarithmic scale.
With the default `scale = "natural"`, data are transformed as

``` math
y = \log_2(\text{value}/\text{reference value}).
```

The default reference value is 1, making the inverse transformation
simply `2^y`. A scientifically meaningful assay reference can instead be
supplied with `reference_value`. This fixed reference is stored with the
fit and used by posterior summaries and response-scale predictions. It
does not depend on the minimum observed value.

For values already on this scale, use `scale = "log2"`; censoring limits
must then also be on the log2 scale. The prepared observation table
retains both `value` and `value_model`.

``` r

prepared <- prepare_epikinetics_data(
  dat,
  formula = ~ infection_history,
  covariate_parameters = "all",
  lower_limit = 5,
  upper_limit = 2560
)
epikinetics_data(prepared)[1:6, ]
#>   source_row participant participant_index observation_time exposure_time
#> 1          1           1                 1       2021-03-10    2021-03-08
#> 2          4           1                 1       2021-03-10    2021-03-08
#> 3          7           1                 1       2021-03-10    2021-03-08
#> 4          2           1                 1       2021-04-15    2021-03-08
#> 5          5           1                 1       2021-04-15    2021-03-08
#> 6          8           1                 1       2021-04-15    2021-03-08
#>   time_since_exposure biomarker biomarker_index    value value_model
#> 1                   2 Ancestral               1 175.9350    7.458899
#> 2                   2     Alpha               2   5.0000    2.321928
#> 3                   2     Delta               3   5.0000    2.321928
#> 4                  38 Ancestral               1 607.5750    9.246919
#> 5                  38     Alpha               2 416.7905    8.703178
#> 6                  38     Delta               3 288.1785    8.170819
#>   lower_limit lower_limit_model upper_limit upper_limit_model censoring
#> 1           5          2.321928        2560          11.32193      none
#> 2           5          2.321928        2560          11.32193      left
#> 3           5          2.321928        2560          11.32193      left
#> 4           5          2.321928        2560          11.32193      none
#> 5           5          2.321928        2560          11.32193      none
#> 6           5          2.321928        2560          11.32193      none
#>   censoring_code infection_history
#> 1              0   Infection naive
#> 2             -1   Infection naive
#> 3             -1   Infection naive
#> 4              0   Infection naive
#> 5              0   Infection naive
#> 6              0   Infection naive
```

## Covariates

Covariates are specified with a one-sided R formula such as
`~ age + sex + infection_history`. They must be participant-level: a
covariate may not change across a participant’s observations. Numeric
variables remain numeric; factors use R’s active contrast options and
their first level is the default reference under treatment contrasts.
Character variables are converted to factors.

Continuous variables enter the design matrix on the scale supplied, so
their coefficients are effects per one input unit and the default
coefficient priors must be interpreted on that scale. For variables such
as age, create an explicit centred or rescaled participant-level column
before preparation when a one-unit effect is not scientifically useful.
This keeps the transformation visible in the prepared data and makes
`newdata` unambiguous.

Use conventional formulas with an intercept. `epikinetics` removes the
design intercept because every biomarker already has a population
parameter. Formulas such as `~ 0 + infection_history` are rejected:
estimating all factor levels in addition to the biomarker population
parameters would be redundant.

The fitted coefficients are shared across biomarkers, as in the original
model. A baseline coefficient is additive on the log2 outcome scale, so
`2^beta` is a response ratio. Coefficients for positive time and rate
parameters are additive on the parameter’s log scale, so `exp(beta)` is
a ratio. `posterior_parameters(level = "regression")` returns both forms
along with the original term, numeric design-column name, represented
factor level, and treatment reference where those labels are well
defined.

The original participant model frame, terms, term assignment, factor
levels, actual contrast matrices, and a design-column-to-term table are
retained. Consequently prediction never requires a user to recreate Stan
dummy values. For example, `prediction_grid(prepared)` returns the
observed factor profiles with continuous predictors fixed at their
participant-level medians.

## Participant hierarchy

`participant_parameters` controls which kinetic quantities receive
participant-level random effects. The default is
`c("baseline", "boost_rate", "early_waning_rate", "late_waning_rate")`.
Peak time and the duration to the early/late waning switch are shared
across participants. This stable structure is the package default for
the motivating application, but remains an explicit and overridable
modelling assumption. Use a scientifically supported subset for another
study, or `participant_parameters = "all"` when richer
within-participant sampling and diagnostics justify the full hierarchy.
The selection is inspectable in
`prepared$mappings$participant_parameters` and as the six-entry
`participant_effect_active` vector in `stan_data(prepared)`.

## Biomarker order

`biomarker_order` supplies one complete user-facing ordering and stores
it in the prepared object. If it is omitted, existing factor levels are
retained; otherwise first appearance in the input data is used. This
same order controls Stan biomarker indices, posterior tables,
predictions, legends, population plots, and individual plots. The
package does not attach pathogen-specific meaning to labels. For the
bundled example, chronological variant order can be made explicit:

``` r

prepare_epikinetics_data(
  dat,
  biomarker_order = c("Ancestral", "Alpha", "Delta")
)
```

## Censoring

The likelihood distinguishes:

- uncensored values, using a Normal log density;
- left-censored values, using the Normal log CDF at their lower limit;
  and
- right-censored values, using the Normal upper-tail probability at
  their upper limit.

`lower_limit` and `upper_limit` independently accept:

- one scalar for all observations;
- a numeric vector with one value per observation;
- a named numeric vector with one limit per biomarker; or
- the name of a row-level numeric data column.

`NA` means no applicable limit for that observation. Without an explicit
`censoring` argument, a value at or below its lower limit is
left-censored and a value at or above its upper limit is right-censored.
Without limits, all values are uncensored. This means ordinary data are
never implicitly censored at their observed minimum or maximum.

If the source data record censoring status, pass a vector or column name
with labels `none`, `left`, or `right` (numeric `0`, `-1`, `1` is also
accepted). Every explicitly censored row must have the relevant limit.
An observation inconsistent with its status and applicable limit is
rejected during preparation. For example, an uncensored observation at
or beyond an upper limit, or a right-censored recorded value below that
limit, produces a row-specific error before Stan is called.

Limits can vary by biomarker:

``` r

biomarkers <- unique(dat$titre_type)
lower <- setNames(rep(5, length(biomarkers)), biomarkers)
upper <- setNames(rep(2560, length(biomarkers)), biomarkers)

prepared <- prepare_epikinetics_data(
  dat,
  lower_limit = lower,
  upper_limit = upper
)
table(epikinetics_data(prepared)$censoring)
#> 
#>  left  none right 
#>   126  2003   126
```

## Inspect the prepared model data

An `epikinetics_data` object keeps the validated input copy, a canonical
labelled observation table, a participant table, the R model frame, the
named participant design matrix, priors, term/design-column mappings,
factor levels/contrasts, conditional-prediction defaults, scale
information, and the exact Stan data list. Input observations are sorted
contiguously by participant for threaded likelihood evaluation without
modifying the user’s data frame.

``` r

prepared
#> Prepared epikinetics model data
#>   Observations: 2255
#>   Participants: 335
#>   Biomarkers:   3 (Ancestral, Alpha, Delta; first appearance order)
#>   Covariates:   none
#>   Random effects: baseline, boost_rate, early_waning_rate, late_waning_rate
#>   Censoring:    none=2003, left=126, right=126
#>   Time range:     0 to 578 since exposure
#>   Model scale:  log2(value / 1); range  2.321928 to 11.321928
#>   Exposure:     one fixed focal exposure per participant
summary(prepared)
#> Prepared epikinetics model-data summary
#> observations participants   biomarkers   covariates 
#>         2255          335            3            0 
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
#> Formula: ~1
#> Transformation: log2(value / 1)
#> Model-matrix columns: none
#> Participant random effects: baseline, boost_rate, early_waning_rate, late_waning_rate
#> Biomarker order (first appearance): Ancestral, Alpha, Delta

# Validated input and model-ready observations.
prepared$input_data[1:3, ]
#>   pid        day last_exp_day titre_type    value infection_history
#> 1   1 2021-03-10   2021-03-08  Ancestral 175.9350   Infection naive
#> 2   1 2021-04-15   2021-03-08  Ancestral 607.5750   Infection naive
#> 3   1 2021-07-08   2021-03-08  Ancestral 179.0463   Infection naive
#>   last_vax_type exp_num
#> 1      BNT162b2       2
#> 2      BNT162b2       2
#> 3      BNT162b2       2
prepared$observations[1:3, ]
#>   source_row participant participant_index observation_time exposure_time
#> 1          1           1                 1       2021-03-10    2021-03-08
#> 2          4           1                 1       2021-03-10    2021-03-08
#> 3          7           1                 1       2021-03-10    2021-03-08
#>   time_since_exposure biomarker biomarker_index   value value_model lower_limit
#> 1                   2 Ancestral               1 175.935    7.458899           5
#> 2                   2     Alpha               2   5.000    2.321928           5
#> 3                   2     Delta               3   5.000    2.321928           5
#>   lower_limit_model upper_limit upper_limit_model censoring censoring_code
#> 1          2.321928        2560          11.32193      none              0
#> 2          2.321928        2560          11.32193      left             -1
#> 3          2.321928        2560          11.32193      left             -1

# Participant indices/covariates and biomarker indices.
prepared$participants[1:3, ]
#>   participant_index participant exposure_time observation_count
#> 1                 1           1    2021-03-08                 9
#> 2                 2           2    2021-01-11                12
#> 3                 3           3    2021-03-23                 6
prepared$model_frame[1:3, , drop = FALSE]
#> data frame with 0 columns and 3 rows
prepared$mappings$biomarker
#>   biomarker_index biomarker observation_count
#> 1               1 Ancestral               734
#> 2               2     Alpha               754
#> 3               3     Delta               767
prepared$mappings$reference_levels
#> named character(0)
prepared$mappings$design_columns
#> [1] design_column   term            variables       level          
#> [5] reference_level label          
#> <0 rows> (or 0-length row.names)

# The formula encoding and the exact list supplied to Stan.
model.matrix(prepared)[1:3, , drop = FALSE]
#>  
#> 1
#> 2
#> 3
prediction_grid(prepared)
#>   .profile
#> 1        1
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
```

`epikinetics_data(prepared)` is a compact accessor for the observation
table, whereas `stan_data(prepared)` returns the exact sampling list.
The direct `prepared$stan_data` component is also available for
interactive debugging. Integer indices need not be supplied by the user,
but they are intentionally visible. The fit updates only the
computational `grainsize` for the requested thread count; after
sampling, `stan_data(fit)` is the literal list used for that run.

## Fit and posterior output

[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md)
returns one `epikinetics_fit` object containing the CmdStanR fit and all
preparation metadata needed for later interpretation. The underlying
`CmdStanMCMC` object remains available through `cmdstan_fit(fit)`; raw
Stan draws are available through `posterior_draws(fit)`.

[`posterior_parameters()`](https://seroanalytics.org/epikinetics/reference/posterior_parameters.md)
provides labelled kinetic quantities at four levels:

| `level` | Interpretation | Main identifiers |
|----|----|----|
| `"population"` | Biomarker-specific population intercept parameters before covariate effects. | `biomarker` |
| `"profile"` | Population parameters after applying a conditional covariate profile, without participant effects. | `.profile`, `biomarker`, and formula variables |
| `"participant"` | Conditional parameters for fitted participants, including their estimated random effects. | `participant`, `biomarker`, and participant covariates |
| `"regression"` | Formula coefficients and their interpretable multiplicative forms. | formula term, design column, level/reference, and kinetic parameter |

With `summary = TRUE`, parameter output is long: `parameter` (or
`quantity` for regression output) names the quantity and `mean`,
`median`, `sd`, `lower`, and `upper` give its posterior summary. With
`summary = FALSE`, one row is returned per posterior draw and the
kinetic quantities occupy separate columns. Derived output includes:

| Column | Meaning |
|----|----|
| `waning_change_time` | Time of the transition from early to late waning. |
| `baseline_response` | Baseline on the natural response scale. |
| `peak_response` | Value at the fitted peak on the response scale. |
| `waning_change_response` | Value at the early/late waning switch. |
| `early_waning_half_life` | Time for a two-fold decline under the early rate. |
| `late_waning_half_life` | Time for a two-fold decline under the late rate. |

The old package referred to `waning_change_response` as a set point. It
is not necessarily stationary: the late waning rate is estimated and may
be positive.

## Trajectory-prediction output

`predict(fit)` returns an `epikinetics_prediction`, which is an ordinary
data frame with plotting metadata attached. Every summarised prediction
contains:

| Column           | Meaning                                                |
|------------------|--------------------------------------------------------|
| `time`           | Time since the focal exposure, in the input time unit. |
| `biomarker`      | User-facing biomarker label in the stored order.       |
| `estimate`       | Alias for the posterior median.                        |
| `mean`           | Posterior mean trajectory value.                       |
| `median`         | Posterior median trajectory value.                     |
| `lower`, `upper` | Requested pointwise posterior interval.                |

Population and new-participant predictions also retain `.profile` and
the original covariate-profile columns. Individual predictions retain
`participant` and the fitted participant’s covariate columns. All
outputs are on the response scale by default; `scale = "model"` requests
the fitted log2 scale.

With `summary = FALSE`, the output instead contains `.draw`, `time`, and
`estimate` together with the same type-specific identifiers. Keeping
`.draw` is essential when calculating quantities involving more than one
time point, biomarker, or participant: summaries must be formed across
complete draw-specific calculations rather than by combining marginal
medians.

The prediction `type` determines the estimand:

| `type` | Random effects included |
|----|----|
| `"population"` | None; conditional on the supplied or default covariate profiles. |
| `"individual"` | Posterior effects for fitted participants. |
| `"new"` | Newly drawn effects from the fitted participant hierarchy. |

Intervals describe uncertainty in the latent expected trajectory unless
`include_observation_noise = TRUE`, in which case they are posterior
predictive intervals for a future observed measurement. Attributes
record the interval probabilities, scale, uncertainty target, censoring
limits, biomarker order, formula, prediction grid, and stored
observations used by the plot methods.

The applied case-study vignette shows how draw-level individual
predictions can be placed on calendar time and aggregated without losing
draw identity.

The package bundles `delta.csv`, `ba2.csv`, and `xbb.csv` under
`extdata` for examples and tests. They are observational examples from
the motivating study, not implicit defaults for modelling or priors.
