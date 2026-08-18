functions {
  /**
   * Map a standard-Normal latent variable to Normal(mean, sd) truncated at 0.
   *
   * This is an inverse-CDF transport over all material prior mass, with a
   * smooth numerical continuation in floating-point tails. It keeps default
   * initial values on the scale of the stated prior instead of interpreting
   * Stan's unconstrained [-2, 2] initialisation directly as a positive kinetic
   * time or rate. The raw standard-Normal is the sampled parameter and the
   * returned value is its deterministic pushforward, so this function does
   * not require a separate Jacobian adjustment.
   */
  real positive_normal_from_raw(real raw, real mean, real sd) {
    real lower_probability = Phi(-mean / sd);
    real lower_tail_boundary = -7;
    real upper_tail_boundary = 8;

    // Outside this range the exact inverse-CDF expression reaches machine
    // precision before the Normal prior has any material mass.  Continue it
    // smoothly: exponentially towards zero at the lower boundary and
    // linearly at the upper boundary.  Values and first derivatives match at
    // each join, while finite raw proposals always produce finite positives.
    if (raw < lower_tail_boundary) {
      real probability_at_boundary = lower_probability
        + (1 - lower_probability) * Phi(lower_tail_boundary);
      real standard_value_at_boundary = inv_Phi(probability_at_boundary);
      real value_at_boundary = mean + sd * standard_value_at_boundary;
      real derivative_at_boundary = sd * (1 - lower_probability)
        * exp(std_normal_lpdf(lower_tail_boundary)
              - std_normal_lpdf(standard_value_at_boundary));
      real log_ratio = (derivative_at_boundary / value_at_boundary)
        * (raw - lower_tail_boundary);

      return value_at_boundary * exp(fmax(log_ratio, -700));
    }

    if (raw > upper_tail_boundary) {
      real standard_value_at_boundary = sqrt(2) * inv_erfc(
        2 * (1 - lower_probability) * Phi(-upper_tail_boundary)
      );
      real derivative_at_boundary = (1 - lower_probability)
        * exp(std_normal_lpdf(upper_tail_boundary)
              - std_normal_lpdf(standard_value_at_boundary));

      return mean + sd * (
        standard_value_at_boundary
        + derivative_at_boundary * (raw - upper_tail_boundary)
      );
    }

    // The direct inverse-CDF expression is accurate for raw <= 0.  For a
    // positive raw value, form the complementary probability instead.  This
    // avoids Phi(raw) rounding to exactly one (at about raw = 8.3), which
    // would otherwise turn an ordinary leapfrog proposal into +infinity.
    if (raw <= 0) {
      real probability = lower_probability
                         + (1 - lower_probability) * Phi(raw);
      return mean + sd * inv_Phi(probability);
    }

    return mean + sd * sqrt(2)
           * inv_erfc(2 * (1 - lower_probability) * Phi(-raw));
  }

  /**
   * Map a standard-Normal latent variable to a half-Normal value over all
   * material prior mass, with a smooth continuation in floating-point tails.
   * This is a prior transport, distinct from Stan's exponential transform for
   * a lower-bounded declaration and from participant-level non-centring.
   */
  real half_normal_from_raw(real raw, real scale) {
    real lower_tail_boundary = -7;
    real upper_tail_boundary = 8;

    if (raw < lower_tail_boundary) {
      real value_at_boundary = inv_Phi(
        0.5 + 0.5 * Phi(lower_tail_boundary)
      );
      real derivative_at_boundary = 0.5
        * exp(std_normal_lpdf(lower_tail_boundary)
              - std_normal_lpdf(value_at_boundary));
      real log_ratio = (derivative_at_boundary / value_at_boundary)
        * (raw - lower_tail_boundary);

      return scale * value_at_boundary * exp(fmax(log_ratio, -700));
    }

    if (raw > upper_tail_boundary) {
      real value_at_boundary = sqrt(2) * inv_erfc(
        Phi(-upper_tail_boundary)
      );
      real derivative_at_boundary = 0.5
        * exp(std_normal_lpdf(upper_tail_boundary)
              - std_normal_lpdf(value_at_boundary));

      return scale * (
        value_at_boundary
        + derivative_at_boundary * (raw - upper_tail_boundary)
      );
    }

    // Use the lower half of the CDF for raw <= 0 and its complementary form
    // for raw > 0.  The two expressions define the same exact half-Normal
    // transport, but neither loses the relevant tail probability through
    // subtraction from one at values HMC can realistically propose.
    if (raw <= 0) {
      return scale * inv_Phi(0.5 + 0.5 * Phi(raw));
    }

    return scale * sqrt(2) * inv_erfc(Phi(-raw));
  }

  /**
   * Expected biomarker value on the model (log2) scale.
   *
   * The curve rises to a peak, wanes at an early rate, then changes to a
   * long-term waning rate. A long-term rate near zero represents a plateau.
   */
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

  /**
   * Stable log probability for a right-censored Normal observation.
   *
   * P(Y >= upper | mu, sigma) = Phi((mu - upper) / sigma). Expressing this
   * through normal_lcdf() avoids normal_lccdf()'s exact-zero approximation
   * when (upper - mu) / sigma is greater than 8.25. That approximation is
   * harmless for many posterior states but can make otherwise valid random
   * initial values have a log density of negative infinity.
   */
  real right_censored_normal_lpdf(real upper_limit, real mu, real sigma) {
    return normal_lcdf(mu | upper_limit, sigma);
  }

  /**
   * Participant-partitioned likelihood for reduce_sum(). Observations are
   * sorted and stored contiguously by participant in the R data preparation
   * code, so each worker reads a compact range of the observation arrays.
   */
  real participant_partial_sum(array[] int participant_slice,
                        int slice_start,
                        int slice_end,
                        array[] int observation_start,
                        array[] int observation_end,
                        array[] int biomarker,
                        vector time,
                        vector value,
                        array[] int censoring,
                        vector lower_limit,
                        vector upper_limit,
                        array[] matrix participant_kinetics,
                        real observation_sd) {
    real lp = 0;

    for (local_index in 1 : size(participant_slice)) {
      int participant = participant_slice[local_index];

      for (n in observation_start[participant] : observation_end[participant]) {
        int k = biomarker[n];
        real mu = kinetics_mean(
          time[n],
          participant_kinetics[1][participant, k],
          participant_kinetics[2][participant, k],
          participant_kinetics[3][participant, k],
          participant_kinetics[4][participant, k],
          participant_kinetics[5][participant, k],
          participant_kinetics[6][participant, k]
        );

        if (censoring[n] == -1) {
          lp += normal_lcdf(lower_limit[n] | mu, observation_sd);
        } else if (censoring[n] == 1) {
          lp += right_censored_normal_lpdf(
            upper_limit[n] | mu, observation_sd
          );
        } else {
          lp += normal_lpdf(value[n] | mu, observation_sd);
        }
      }
    }

    return lp;
  }
}

data {
  int<lower=1> N_observations;
  int<lower=1> N_participants;
  int<lower=1> N_biomarkers;
  int<lower=0> N_covariates;

  array[N_observations] int<lower=1, upper=N_biomarkers> biomarker;
  vector<lower=0>[N_observations] time;
  vector[N_observations] value;
  array[N_observations] int<lower=-1, upper=1> censoring;
  vector[N_observations] lower_limit;
  vector[N_observations] upper_limit;

  array[N_participants] int<lower=1, upper=N_observations> observation_start;
  array[N_participants] int<lower=1, upper=N_observations> observation_end;
  array[N_participants] int<lower=1, upper=N_participants> participant_sequence;

  matrix[N_participants, N_covariates] X;
  array[6] int<lower=0, upper=1> covariate_active;
  array[6] int<lower=0, upper=1> participant_effect_active;
  int<lower=1> grainsize;

  // Priors use the public parameter order documented by epikinetics_priors().
  vector[6] population_prior_mean;
  vector<lower=0>[6] population_prior_sd;
  vector<lower=0>[6] participant_sd_prior_scale;
  vector<lower=0>[6] covariate_prior_scale;
  real<lower=0> observation_sd_prior_scale;
}

parameters {
  // All raw parameters are on standard-Normal scales. The transformations
  // below preserve the public priors exactly and give reliable default
  // initial values for quantities whose scientific scales differ by orders of
  // magnitude (for example, a 50-day duration and a 0.002/day late rate).
  vector[N_biomarkers] population_baseline_raw;
  vector[N_biomarkers] population_time_to_peak_raw;
  vector[N_biomarkers] population_waning_duration_raw;
  vector[N_biomarkers] population_boost_rate_raw;
  vector[N_biomarkers] population_early_waning_rate_raw;
  vector[N_biomarkers] population_late_waning_rate_raw;

  // Participant effects are non-centred. As in the original model, a
  // participant's standardised deviation is shared across biomarkers and is
  // scaled by a biomarker-specific standard deviation.
  vector[N_biomarkers * participant_effect_active[1]] participant_sd_baseline_raw;
  vector[N_biomarkers * participant_effect_active[2]] participant_sd_time_to_peak_raw;
  vector[N_biomarkers * participant_effect_active[3]] participant_sd_waning_duration_raw;
  vector[N_biomarkers * participant_effect_active[4]] participant_sd_boost_rate_raw;
  vector[N_biomarkers * participant_effect_active[5]] participant_sd_early_waning_rate_raw;
  vector[N_biomarkers * participant_effect_active[6]] participant_sd_late_waning_rate_raw;

  vector[N_participants * participant_effect_active[1]] z_baseline;
  vector[N_participants * participant_effect_active[2]] z_time_to_peak;
  vector[N_participants * participant_effect_active[3]] z_waning_duration;
  vector[N_participants * participant_effect_active[4]] z_boost_rate;
  vector[N_participants * participant_effect_active[5]] z_early_waning_rate;
  vector[N_participants * participant_effect_active[6]] z_late_waning_rate;

  // Baseline effects are additive on the log2 outcome scale. Effects on
  // positive time and rate parameters are additive on their log scale.
  vector[N_covariates * covariate_active[1]] beta_baseline_raw;
  vector[N_covariates * covariate_active[2]] beta_time_to_peak_raw;
  vector[N_covariates * covariate_active[3]] beta_waning_duration_raw;
  vector[N_covariates * covariate_active[4]] beta_boost_rate_raw;
  vector[N_covariates * covariate_active[5]] beta_early_waning_rate_raw;
  vector[N_covariates * covariate_active[6]] beta_late_waning_rate_raw;

  real observation_sd_raw;
}

transformed parameters {
  vector[N_biomarkers] population_baseline = population_prior_mean[1]
    + population_prior_sd[1] * population_baseline_raw;
  vector[N_biomarkers] population_time_to_peak;
  vector[N_biomarkers] population_waning_duration;
  vector[N_biomarkers] population_boost_rate;
  vector[N_biomarkers] population_early_waning_rate;
  vector[N_biomarkers] population_late_waning_rate;

  vector[N_biomarkers] participant_sd_baseline = zeros_vector(N_biomarkers);
  vector[N_biomarkers] participant_sd_time_to_peak = zeros_vector(N_biomarkers);
  vector[N_biomarkers] participant_sd_waning_duration = zeros_vector(N_biomarkers);
  vector[N_biomarkers] participant_sd_boost_rate = zeros_vector(N_biomarkers);
  vector[N_biomarkers] participant_sd_early_waning_rate = zeros_vector(N_biomarkers);
  vector[N_biomarkers] participant_sd_late_waning_rate = zeros_vector(N_biomarkers);

  vector[N_covariates * covariate_active[1]] beta_baseline =
    covariate_prior_scale[1] * beta_baseline_raw;
  vector[N_covariates * covariate_active[2]] beta_time_to_peak =
    covariate_prior_scale[2] * beta_time_to_peak_raw;
  vector[N_covariates * covariate_active[3]] beta_waning_duration =
    covariate_prior_scale[3] * beta_waning_duration_raw;
  vector[N_covariates * covariate_active[4]] beta_boost_rate =
    covariate_prior_scale[4] * beta_boost_rate_raw;
  vector[N_covariates * covariate_active[5]] beta_early_waning_rate =
    covariate_prior_scale[5] * beta_early_waning_rate_raw;
  vector[N_covariates * covariate_active[6]] beta_late_waning_rate =
    covariate_prior_scale[6] * beta_late_waning_rate_raw;

  real observation_sd = half_normal_from_raw(
    observation_sd_raw, observation_sd_prior_scale
  );

  for (k in 1 : N_biomarkers) {
    population_time_to_peak[k] = positive_normal_from_raw(
      population_time_to_peak_raw[k],
      population_prior_mean[2], population_prior_sd[2]
    );
    population_waning_duration[k] = positive_normal_from_raw(
      population_waning_duration_raw[k],
      population_prior_mean[3], population_prior_sd[3]
    );
    population_boost_rate[k] = positive_normal_from_raw(
      population_boost_rate_raw[k],
      population_prior_mean[4], population_prior_sd[4]
    );
    population_early_waning_rate[k] = positive_normal_from_raw(
      population_early_waning_rate_raw[k],
      population_prior_mean[5], population_prior_sd[5]
    );
    population_late_waning_rate[k] = positive_normal_from_raw(
      population_late_waning_rate_raw[k],
      population_prior_mean[6], population_prior_sd[6]
    );

    if (participant_effect_active[1]) {
      participant_sd_baseline[k] = half_normal_from_raw(
        participant_sd_baseline_raw[k], participant_sd_prior_scale[1]
      );
    }
    if (participant_effect_active[2]) {
      participant_sd_time_to_peak[k] = half_normal_from_raw(
        participant_sd_time_to_peak_raw[k], participant_sd_prior_scale[2]
      );
    }
    if (participant_effect_active[3]) {
      participant_sd_waning_duration[k] = half_normal_from_raw(
        participant_sd_waning_duration_raw[k], participant_sd_prior_scale[3]
      );
    }
    if (participant_effect_active[4]) {
      participant_sd_boost_rate[k] = half_normal_from_raw(
        participant_sd_boost_rate_raw[k], participant_sd_prior_scale[4]
      );
    }
    if (participant_effect_active[5]) {
      participant_sd_early_waning_rate[k] = half_normal_from_raw(
        participant_sd_early_waning_rate_raw[k], participant_sd_prior_scale[5]
      );
    }
    if (participant_effect_active[6]) {
      participant_sd_late_waning_rate[k] = half_normal_from_raw(
        participant_sd_late_waning_rate_raw[k], participant_sd_prior_scale[6]
      );
    }
  }

  vector[N_biomarkers] population_waning_change_time =
    population_time_to_peak + population_waning_duration;
}

model {
  array[6] matrix[N_participants, N_biomarkers] participant_kinetics;

  vector[N_participants] baseline_covariate_effect = zeros_vector(N_participants);
  vector[N_participants] time_to_peak_covariate_effect = zeros_vector(N_participants);
  vector[N_participants] waning_duration_covariate_effect = zeros_vector(N_participants);
  vector[N_participants] boost_rate_covariate_effect = zeros_vector(N_participants);
  vector[N_participants] early_waning_covariate_effect = zeros_vector(N_participants);
  vector[N_participants] late_waning_covariate_effect = zeros_vector(N_participants);

  if (covariate_active[1]) baseline_covariate_effect = X * beta_baseline;
  if (covariate_active[2]) time_to_peak_covariate_effect = X * beta_time_to_peak;
  if (covariate_active[3]) waning_duration_covariate_effect = X * beta_waning_duration;
  if (covariate_active[4]) boost_rate_covariate_effect = X * beta_boost_rate;
  if (covariate_active[5]) {
    early_waning_covariate_effect = X * beta_early_waning_rate;
  }
  if (covariate_active[6]) {
    late_waning_covariate_effect = X * beta_late_waning_rate;
  }

  for (participant in 1 : N_participants) {
    for (k in 1 : N_biomarkers) {
      real baseline_random_effect = 0;
      real time_to_peak_random_effect = 0;
      real waning_duration_random_effect = 0;
      real boost_rate_random_effect = 0;
      real early_waning_random_effect = 0;
      real late_waning_random_effect = 0;

      if (participant_effect_active[1]) {
        baseline_random_effect = participant_sd_baseline[k]
          * z_baseline[participant];
      }
      if (participant_effect_active[2]) {
        time_to_peak_random_effect = participant_sd_time_to_peak[k]
          * z_time_to_peak[participant];
      }
      if (participant_effect_active[3]) {
        waning_duration_random_effect = participant_sd_waning_duration[k]
          * z_waning_duration[participant];
      }
      if (participant_effect_active[4]) {
        boost_rate_random_effect = participant_sd_boost_rate[k]
          * z_boost_rate[participant];
      }
      if (participant_effect_active[5]) {
        early_waning_random_effect = participant_sd_early_waning_rate[k]
          * z_early_waning_rate[participant];
      }
      if (participant_effect_active[6]) {
        late_waning_random_effect = participant_sd_late_waning_rate[k]
          * z_late_waning_rate[participant];
      }

      real time_to_peak = population_time_to_peak[k] * exp(
        time_to_peak_covariate_effect[participant]
        + time_to_peak_random_effect
      );
      real waning_duration = population_waning_duration[k] * exp(
        waning_duration_covariate_effect[participant]
        + waning_duration_random_effect
      );

      participant_kinetics[1][participant, k] = population_baseline[k]
        + baseline_covariate_effect[participant]
        + baseline_random_effect;
      participant_kinetics[2][participant, k] = time_to_peak;
      participant_kinetics[3][participant, k] = time_to_peak + waning_duration;
      participant_kinetics[4][participant, k] = population_boost_rate[k] * exp(
        boost_rate_covariate_effect[participant]
        + boost_rate_random_effect
      );
      participant_kinetics[5][participant, k] = population_early_waning_rate[k] * exp(
        early_waning_covariate_effect[participant]
        + early_waning_random_effect
      );
      participant_kinetics[6][participant, k] = population_late_waning_rate[k] * exp(
        late_waning_covariate_effect[participant]
        + late_waning_random_effect
      );
    }
  }

  population_baseline_raw ~ std_normal();
  population_time_to_peak_raw ~ std_normal();
  population_waning_duration_raw ~ std_normal();
  population_boost_rate_raw ~ std_normal();
  population_early_waning_rate_raw ~ std_normal();
  population_late_waning_rate_raw ~ std_normal();

  participant_sd_baseline_raw ~ std_normal();
  participant_sd_time_to_peak_raw ~ std_normal();
  participant_sd_waning_duration_raw ~ std_normal();
  participant_sd_boost_rate_raw ~ std_normal();
  participant_sd_early_waning_rate_raw ~ std_normal();
  participant_sd_late_waning_rate_raw ~ std_normal();

  z_baseline ~ std_normal();
  z_time_to_peak ~ std_normal();
  z_waning_duration ~ std_normal();
  z_boost_rate ~ std_normal();
  z_early_waning_rate ~ std_normal();
  z_late_waning_rate ~ std_normal();

  beta_baseline_raw ~ std_normal();
  beta_time_to_peak_raw ~ std_normal();
  beta_waning_duration_raw ~ std_normal();
  beta_boost_rate_raw ~ std_normal();
  beta_early_waning_rate_raw ~ std_normal();
  beta_late_waning_rate_raw ~ std_normal();

  observation_sd_raw ~ std_normal();

  target += reduce_sum(
    participant_partial_sum,
    participant_sequence,
    grainsize,
    observation_start,
    observation_end,
    biomarker,
    time,
    value,
    censoring,
    lower_limit,
    upper_limit,
    participant_kinetics,
    observation_sd
  );
}
