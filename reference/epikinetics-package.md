# epikinetics: Bayesian hierarchical biomarker kinetics

`epikinetics` provides a functional R interface to a threaded CmdStan
model for longitudinal biomarker measurements following one focal
exposure. The main workflow is
[`prepare_epikinetics_data()`](https://seroanalytics.org/epikinetics/reference/prepare_epikinetics_data.md),
[`fit_epikinetics()`](https://seroanalytics.org/epikinetics/reference/fit_epikinetics.md),
[`diagnose_epikinetics()`](https://seroanalytics.org/epikinetics/reference/diagnose_epikinetics.md),
[`posterior_parameters()`](https://seroanalytics.org/epikinetics/reference/posterior_parameters.md),
and
[`predict.epikinetics_fit()`](https://seroanalytics.org/epikinetics/reference/predict.epikinetics_fit.md).
[`prediction_grid()`](https://seroanalytics.org/epikinetics/reference/prediction_grid.md)
carries formula metadata into conditional population trajectories;
[`plot_individual()`](https://seroanalytics.org/epikinetics/reference/plot_individual.md)
and
[`save_individual_plots()`](https://seroanalytics.org/epikinetics/reference/save_individual_plots.md)
support fitted-participant inspection. Prepared data and the exact Stan
list are inspectable before sampling. The returned S3 fit retains direct
access to its CmdStanR fit through
[`cmdstan_fit()`](https://seroanalytics.org/epikinetics/reference/cmdstan_fit.md).

## See also

Useful links:

- <https://seroanalytics.org/epikinetics/>

- <https://github.com/seroanalytics/epikinetics>

- Report bugs at <https://github.com/seroanalytics/epikinetics/issues>

## Author

**Maintainer**: Alex Hill <alex.hill@gmail.com>

Authors:

- Alex Hill <alex.hill@gmail.com>

- Timothy Russell <timothy.russell@lshtm.ac.uk>
