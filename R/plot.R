#' @title Simulate biomarker kinetics predicted by the given biokinetics priors
#' and optionally compare to a dataset.
#' @export
#' @description Simulate trajectories by drawing random samples from the given
#' priors for each parameter in the biokinetics model.
#' @return A ggplot2 object.
#' @param priors A named list of type 'biokinetics_priors'.
#' @param tmax Integer. The number of time points in each simulated trajectory. Default 150.
#' @param n_draws Integer. The number of trajectories to simulate. Default 2000.
#' @param data Optional data.frame with columns t_since_last_exp and value. The raw data to compare to.
plot_prior_predictive <- function(priors,
                                  tmax = 150,
                                  n_draws = 2000,
                                  data = NULL) {
  validate_priors(priors)
  if (!is.null(data)) {
    validate_required_cols(data, c("t_since_last_exp", "value"))
  }
  params <- data.table(
    t0 = rnorm(n_draws, priors$mu_t0, priors$sigma_t0), # titre value at t0
    tp = rnorm(n_draws, priors$mu_tp, priors$sigma_tp), # time of peak
    ts = rnorm(n_draws, priors$mu_ts, priors$sigma_ts), # time of set point
    m1 = rnorm(n_draws, priors$mu_m1, priors$sigma_m1), # gradient 1
    m2 = rnorm(n_draws, priors$mu_m2, priors$sigma_m2), # gradient 2
    m3 = rnorm(n_draws, priors$mu_m3, priors$sigma_m3) # gradient 3
  )

  times <- data.table(t = 1:tmax)
  params_and_times <- times[, as.list(params), by = times]

  params_and_times[, mu := biokinetics_simulate_trajectory(t, t0, tp, ts, m1, m2, m3),
                     by = c("t", "t0", "tp", "ts", "m1", "m2", "m3")]

  summary <- params_and_times[, .(me = stats::quantile(mu, 0.5, names = FALSE),
                                  lo = stats::quantile(mu, 0.025, names = FALSE),
                                  hi = stats::quantile(mu, 0.975, names = FALSE)), by = t]

  plot <- ggplot(summary) +
    geom_line(aes(x = t, y = me)) +
    geom_ribbon(aes(x = t, ymin = lo, ymax = hi), alpha = 0.5)

  if (!is.null(data)) {
    plot <- plot + geom_point(data = data, aes(x = t_since_last_exp, y = value))
  }
  plot
}

plot_data <- function(data) {
  validate_required_cols(data, c("t_since_last_exp", "value", "titre_type"))
  ggplot(data) +
    geom_point(aes(x = t_since_last_exp, y = value, colour = titre_type)) +
    geom_smooth(aes(x = t_since_last_exp, y = value, colour = titre_type)) +
    facet_wrap(~titre_type) +
    guides(colour = guide_legend(title = "Titre type"))
}
