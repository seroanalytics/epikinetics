.epikinetics_parameters <- c(
  "baseline",
  "time_to_peak",
  "waning_duration",
  "boost_rate",
  "early_waning_rate",
  "late_waning_rate"
)

.epikinetics_positive_parameters <- setdiff(
  .epikinetics_parameters,
  "baseline"
)

.population_prefixes <- stats::setNames(
  paste0("population_", .epikinetics_parameters),
  .epikinetics_parameters
)

.participant_sd_prefixes <- stats::setNames(
  paste0("participant_sd_", .epikinetics_parameters),
  .epikinetics_parameters
)

.z_prefixes <- stats::setNames(
  paste0("z_", .epikinetics_parameters),
  .epikinetics_parameters
)

.beta_prefixes <- stats::setNames(
  paste0("beta_", .epikinetics_parameters),
  .epikinetics_parameters
)
