# Extract raw posterior draws

Extract raw posterior draws

## Usage

``` r
posterior_draws(
  x,
  variables = NULL,
  format = c("draws_df", "draws_matrix", "draws_array", "draws_list")
)
```

## Arguments

- x:

  An `epikinetics_fit` object.

- variables:

  Optional Stan variable names. Base names select all indexed elements,
  as in CmdStanR.

- format:

  One of the posterior package draw formats.

## Value

A posterior draws object.
