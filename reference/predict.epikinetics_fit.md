# Predict biomarker trajectories

Predict biomarker trajectories

## Usage

``` r
# S3 method for class 'epikinetics_fit'
predict(
  object,
  newdata = NULL,
  times = 0:150,
  type = c("population", "individual", "participant", "new"),
  participants = NULL,
  biomarkers = NULL,
  summary = TRUE,
  ndraws = NULL,
  chunk_size = 20L,
  max_rows = 5e+06,
  probs = c(0.025, 0.975),
  scale = c("response", "model"),
  include_observation_noise = FALSE,
  seed = NULL,
  ...
)
```

## Arguments

- object:

  An `epikinetics_fit` object.

- newdata:

  Participant-level covariate profiles for population or new-
  participant predictions. `NULL` uses
  [`prediction_grid()`](https://seroanalytics.org/epikinetics/reference/prediction_grid.md):
  observed categorical combinations with continuous predictors fixed at
  their participant-level medians. The resulting predictions are
  conditional, not marginalised over the fitted covariate distribution.

- times:

  Non-negative times since exposure.

- type:

  `"population"` excludes participant variation; `"individual"` uses
  posterior effects for fitted participants; and `"new"` draws new
  participant effects for each posterior draw/profile. `"participant"`
  is retained as an alias for `"individual"`.

- participants:

  Optional fitted participant ids for individual prediction. `NULL`
  selects all fitted participants.

- biomarkers:

  Optional biomarker subset. The stored biomarker order is retained.

- summary:

  Return posterior summaries rather than individual draws.

- ndraws:

  Optional maximum number of posterior draws.

- chunk_size:

  Number of fitted participants processed together. Summary predictions
  are calculated in bounded chunks and do not materialise the complete
  participant-by-draw-by-time table.

- max_rows:

  Safety limit for unsummarised output. Subset participants, biomarkers,
  times, or posterior draws to stay below this value; use `Inf` only
  when the resulting memory requirement has been considered explicitly.

- probs:

  Lower and upper interval probabilities.

- scale:

  Return values on the natural response or model log2 scale.

- include_observation_noise:

  Include residual measurement error.

- seed:

  Optional seed for new-participant effects or observation noise.

- ...:

  Reserved for future methods.

## Value

An `epikinetics_prediction` data frame. Unsummarised predictions include
`.draw`; summaries include `estimate` (an alias for `median`), `mean`,
`median`, `lower`, and `upper`. Without observation noise, intervals
describe uncertainty in the latent expected trajectory. With observation
noise, they are posterior predictive intervals for a future measurement.
