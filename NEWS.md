# epikinetics 0.1.0.9000

This development version is a deliberate, breaking redesign of the original
package.

## Interface

* Replaced the public mutable `biokinetics` R6 workflow with
  `prepare_epikinetics_data()`, `fit_epikinetics()`, and an S3
  `epikinetics_fit` object.
* Made preparation a required, independently inspectable operation. Prepared
  objects expose validated input, transformed observations, censoring,
  participant/biomarker mappings, model matrices, factor/reference metadata,
  and the exact Stan list through `stan_data()`.
* Added concise `print()` and detailed `summary()` methods for prepared data,
  plus strict pre-Stan structural and censoring validation.
* Added standard `print()`, `summary()`, `plot()`, and `predict()` methods,
  labelled posterior extraction, sampling diagnostics, and direct CmdStanR
  access.
* Made the kinetic parameters affected by a covariate formula explicit through
  `covariate_parameters`; inactive parameter/formula combinations no longer
  create unused regression coefficients.
* Added `prediction_grid()`. Default population predictions now carry the
  fitted formula forward: they use observed categorical combinations and
  participant-level medians for continuous predictors, while explicit
  `newdata` and Cartesian factor grids remain available.
* Retained the participant model frame, terms, assignments, contrast matrices,
  factor/reference levels, and labelled design-column mappings. Prediction
  plots overlay biomarkers by colour and facet by categorical profile without
  hard-coded variable names, and regression summaries expose R-level term and
  contrast labels.
* Added explicit biomarker ordering, posterior means and medians in trajectory
  summaries, publication-oriented plot defaults, assay-limit lines, and
  distinct symbols for censored observations.
* Added chunked R-side individual trajectory prediction, `plot_individual()`,
  and reusable PNG/PDF/multi-page-PDF export through
  `save_individual_plots()`.
* Labelled trajectory intervals as latent-mean credible intervals or posterior
  predictive observation intervals, and warn before prediction when retained
  chains have non-finite E-BFMI or other serious sampler problems.
* Replaced `biokinetics_priors()` with scientifically named
  `epikinetics_priors()`.
* Removed the Shiny input inspector and the old specialised trajectory,
  stationary-point, scale-conversion, and data-merging functions. Their core
  roles are covered by data preparation, posterior extraction, and prediction.

## Model and computation

* Retained the continuous three-segment log2-linear scientific curve while
  renaming its parameters and documenting its actual early/late waning
  interpretation.
* Parameterised the waning transition as positive time to peak plus positive
  waning duration. Positive kinetic quantities now use multiplicative
  covariate and participant effects, preventing invalid times and rate signs.
* Removed the artificial zero floor on the log2 curve.
* Added participant-partitioned `reduce_sum` likelihood evaluation and compiled
  the model with Stan threading enabled.
* Re-expressed the right-censored Normal upper-tail likelihood with the exact
  stable lower-tail identity. This prevents Stan Math's extreme-tail
  `normal_lccdf()` cutoff from producing `log(0)` at otherwise valid default
  initial values.
* Stopped saving large participant transformed-parameter matrices in CmdStan
  output; labelled participant quantities are reconstructed in R.
* Reparameterised truncated-Normal and half-Normal priors through
  standard-Normal inverse-CDF transports. This preserves the intended priors
  over all represented probability mass while preventing Stan's default
  unconstrained initial values from becoming
  implausibly large rates, durations, and hierarchical standard deviations.
  The model documentation now distinguishes these prior transports from
  Stan's lower-bound transform and from participant-effect non-centring.
* Made participant random effects explicit through `participant_parameters`.
  The final default fits baseline, boost-rate, early-waning-rate, and
  late-waning-rate heterogeneity while giving peak and switch timing no
  residual participant random effect after conditioning on covariates. The
  full six-effect hierarchy remains available with `"all"`. This scientifically
  motivated structure produced stable geometry for the motivating application.
* Stabilised the prior transports in floating-point tails with complementary
  probabilities and smooth value/gradient-matched continuations, preventing
  ordinary leapfrog probes from producing exact zero or infinity.
* Expanded chain diagnostics to distinguish low from non-finite E-BFMI and to
  identify constant retained energy, the signature of a frozen chain.
* Raised the fitting default to `adapt_delta = 0.9` and
  `max_treedepth = 12`, reflecting the nonlinear hierarchical geometry.

## Installation

* Removed the package C++ layer, `instantiate`, install-time model compilation,
  and load-time CmdStan installation.
* CmdStan setup is now explicit. The threaded Stan executable is compiled on
  first fit and cached per user.
* Moved the three bundled public datasets to conventional CSV files under
  `inst/extdata`.

## Documentation

* Added the package logo to the README and pkgdown site metadata.
* Restored the applied Delta-wave case study using the functional API,
  including conditional population curves, peak/switch summaries,
  calendar-time cohort trajectories, and exposure-timing counterfactuals.
* Added a dedicated priors vignette and an explicit workflow-oriented article
  order. Prior trajectory plots now use the same log2-spaced response axis as
  posterior plots and state which sources of variation they exclude.
* Added shared responsive vignette styling, deliberately proportioned SVG
  figures, accessible captions, compact tables of contents, and overflow-safe
  code, tables, and mathematical displays.
