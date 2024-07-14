scova_simulate_trajectory <- function(t, t0, tp, ts, m1, m2, m3) {
  mu <- t0
  if (t < tp) {
    mu <- mu + m1 * t
  } else if (t <= ts) {
    mu <- mu + m1 * tp + m2 * (t - tp)
  } else if (t > ts) {
    mu <- mu + m1 * tp + m2 * (ts - tp) + m3 * (t - ts)
  }
  max(mu, 0)
}

scova_simulate_trajectories <- function(dat) {
  data.table::setDT(simulate_trajectories_cpp(dat))
}
