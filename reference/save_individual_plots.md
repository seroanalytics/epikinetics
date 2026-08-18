# Save fitted individual trajectory plots

Computes individual trajectory summaries once, then reuses them for each
participant figure. PNG and one-file-per-participant PDF output are
supported, as is a single multi-page PDF.

## Usage

``` r
save_individual_plots(
  x,
  path,
  participants = NULL,
  format = c("png", "pdf"),
  multipage = FALSE,
  file = "individual-trajectories.pdf",
  times = 0:150,
  ndraws = 500,
  probs = c(0.025, 0.975),
  scale = c("response", "model"),
  biomarkers = NULL,
  central = c("median", "mean"),
  width = 7.2,
  height = 4.8,
  dpi = 300,
  overwrite = FALSE
)
```

## Arguments

- x:

  An `epikinetics_fit` or summarised individual prediction object.

- path:

  Output directory, created if necessary.

- participants:

  Optional participant subset.

- format:

  `"png"` or `"pdf"`.

- multipage:

  For PDF output, write one multi-page file rather than one file per
  participant.

- file:

  Filename for a multi-page PDF.

- times, ndraws, probs, scale, biomarkers:

  Passed to [`predict()`](https://rdrr.io/r/stats/predict.html) when `x`
  is a fit.

- central:

  Plot the posterior median or mean trajectory.

- width, height:

  Figure dimensions in inches.

- dpi:

  Raster resolution for PNG output.

- overwrite:

  Replace existing output files.

## Value

The output paths, invisibly.
