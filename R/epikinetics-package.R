#' epikinetics: Bayesian hierarchical biomarker kinetics
#'
#' `epikinetics` provides a functional R interface to a threaded CmdStan model
#' for longitudinal biomarker measurements following one focal exposure. The
#' main workflow is [prepare_epikinetics_data()], [fit_epikinetics()],
#' [diagnose_epikinetics()], [posterior_parameters()], and
#' [predict.epikinetics_fit()]. [prediction_grid()] carries formula metadata
#' into conditional population trajectories; [plot_individual()] and
#' [save_individual_plots()] support fitted-participant inspection. Prepared
#' data and the exact Stan list are inspectable before sampling. The returned
#' S3 fit retains direct access to its CmdStanR fit through [cmdstan_fit()].
#'
#' @keywords internal
"_PACKAGE"
