functions {
  real kinetics_mean(real time,
                     real baseline,
                     real time_to_peak,
                     real waning_change_time,
                     real boost_rate,
                     real early_waning_rate,
                     real late_waning_rate) {
    if (time <= time_to_peak) {
      return baseline + boost_rate * time;
    }
    if (time <= waning_change_time) {
      return baseline
             + boost_rate * time_to_peak
             - early_waning_rate * (time - time_to_peak);
    }
    return baseline
           + boost_rate * time_to_peak
           - early_waning_rate * (waning_change_time - time_to_peak)
           - late_waning_rate * (time - waning_change_time);
  }
}
data {
  int<lower=1> N;
  vector<lower=0>[N] time;
  real baseline;
  real<lower=0> time_to_peak;
  real<lower=0> waning_change_time;
  real<lower=0> boost_rate;
  real<lower=0> early_waning_rate;
  real<lower=0> late_waning_rate;
}
model {}
generated quantities {
  vector[N] expected_value;
  for (n in 1:N) {
    expected_value[n] = kinetics_mean(
      time[n], baseline, time_to_peak, waning_change_time,
      boost_rate, early_waning_rate, late_waning_rate
    );
  }
}
