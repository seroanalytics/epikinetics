# Articles

### Start here

- [Getting started with
  epikinetics](https://seroanalytics.org/epikinetics/articles/getting-started.md):

  Prepare longitudinal biomarker data, fit the model, check diagnostics,
  and plot conditional population kinetics.

### Using the model

- [Data](https://seroanalytics.org/epikinetics/articles/data.md):

  Structure, prepare, transform, and inspect longitudinal biomarker
  data.

- [Covariates](https://seroanalytics.org/epikinetics/articles/covariates.md):

  Specify participant-level predictors and understand how they enter the
  hierarchical kinetics model.

- [Censoring](https://seroanalytics.org/epikinetics/articles/censoring.md):

  Supply assay limits and understand lower-, upper-, and uncensored
  contributions to the likelihood.

- [Fitting the
  model](https://seroanalytics.org/epikinetics/articles/fitting.md):

  Compile and sample the Stan model with parallel chains and
  within-chain threading.

- [Population-level
  kinetics](https://seroanalytics.org/epikinetics/articles/population-kinetics.md):

  Predict conditional population trajectories for biomarkers and
  covariate profiles.

- [Individual-level
  kinetics](https://seroanalytics.org/epikinetics/articles/individual-kinetics.md):

  Reconstruct fitted participants, compare latent curves with
  observations, and export participant plots.

- [Diagnostics](https://seroanalytics.org/epikinetics/articles/diagnostics.md):

  Screen sampling behaviour and inspect the underlying CmdStanR fit
  before interpreting posterior results.

### Examples

- [Case study: SARS-CoV-2 Delta-wave neutralising
  antibodies](https://seroanalytics.org/epikinetics/articles/case-study.md):

  Reproduce the motivating Delta-wave population, participant,
  calendar-time, and exposure-timing analyses with the modern interface.

### Statistical details

- [Kinetics model and statistical
  structure](https://seroanalytics.org/epikinetics/articles/model.md):

  The piecewise kinetic curve, hierarchical effects, censoring
  likelihood, constraints, and threaded Stan implementation.
