functions {
  vector boost_wane_ind(vector t, matrix t0, matrix tp, matrix ts, matrix m1,
                        matrix m2, matrix m3, array[] int titre_type,
                        array[] int id) {
    int N = num_elements(t);
    vector[N] mu;
    
    for (n in 1 : N) {
      mu[n] = t0[id[n], titre_type[n]];
      
      if (t[n] < tp[id[n], titre_type[n]]) {
        mu[n] += (m1[id[n], titre_type[n]])*t[n];
      } else if (t[n] <= ts[id[n], titre_type[n]]) {
        mu[n] += (m1[id[n], titre_type[n]])*(tp[id[n], titre_type[n]]) +
                 (m2[id[n], titre_type[n]])*(t[n] - (tp[id[n], titre_type[n]]));
      } else {
        mu[n] += (m1[id[n], titre_type[n]])*(tp[id[n], titre_type[n]]) +
                 (m2[id[n], titre_type[n]])*(ts[id[n], titre_type[n]] -
                 tp[id[n], titre_type[n]]) +
                 (m3[id[n], titre_type[n]])*(t[n] - ts[id[n], titre_type[n]]);
      }
      mu[n] = fmax(mu[n], 0);
    }
    return mu;
  }
}
data {
  // observation data
  int N; // Number of observations
  int N_events; // Number of exposure events
  int K; // Number of titre types
  real upper_limit; // Upper detection limit
  real lower_limit; // Lower detection limit
  array[N] int<lower=1, upper=K> titre_type; // Titre type for each observation
  vector[N] t; // Time for each observation
  vector[N] value; // Observed titre values
  array[N] int<lower=-1, upper=1> censored; // Censoring indicator: -1 for lo, 1 for upper, 0 for none
  
  // Indices for different censoring scenarios
  int N_uncens; // number of uncensored observations
  int N_lo; // number of lower censored observations
  int N_hi; // number of upper censored observations
  
  array[N_uncens] int uncens_idx;
  array[N_lo] int cens_lo_idx;
  array[N_hi] int cens_hi_idx;
  
  // Standard deviation of effect size parameters
  real<lower=0> preds_sd;
  
  // id structure
  array[N] int id; // individual id
  
  // adding design matrix and number of predictors
  int P; // Number of predictors
  matrix[N_events, P] X; // Design matrix
  
  // covariate-level prior mean values
  real mu_t0;
  real mu_tp;
  real mu_ts;
  real mu_m1;
  real mu_m2;
  real mu_m3;
  
  // covariate-level prior SD values
  real<lower=0> sigma_t0;
  real<lower=0> sigma_tp;
  real<lower=0> sigma_ts;
  real<lower=0> sigma_m1;
  real<lower=0> sigma_m2;
  real<lower=0> sigma_m3;
}
parameters {
  // population-level parameters
  vector<lower=0>[K] t0_pop;
  vector<lower=0>[K] tp_pop;
  vector<lower=0>[K] ts_pop_delta;
  vector<lower=0>[K] m1_pop;
  vector<upper=0>[K] m2_pop;
  vector<upper=0>[K] m3_pop;
  
  vector<lower=0>[K] sigma_t0_ind;
  vector<lower=0>[K] sigma_tp_ind;
  vector<lower=0>[K] sigma_ts_ind;
  vector<lower=0>[K] sigma_m1_ind;
  vector<lower=0>[K] sigma_m2_ind;
  vector<lower=0>[K] sigma_m3_ind;
  
  // individual-level non-centered parameters
  vector[N_events] z_t0;
  vector[N_events] z_tp;
  vector[N_events] z_ts;
  vector[N_events] z_m1;
  vector[N_events] z_m2;
  vector[N_events] z_m3;
  
  // beta coefficients, one vector per process parameter
  vector[P] beta_t0; // Coefficients for covariates corresponding to t0
  vector[P] beta_tp; // Coefficients for covariates corresponding to tp
  vector[P] beta_ts; // Coefficients for covariates corresponding to ts
  vector[P] beta_m1; // Coefficients for covariates corresponding to m1
  vector<upper=0>[P] beta_m2; // Coefficients for covariates corresponding to m2
  vector<upper=0>[P] beta_m3; // Coefficients for covariates corresponding to m3
  
  // overall measurement error
  real<lower=0> sigma;
}

transformed parameters {
  vector[K] ts_pop = tp_pop + ts_pop_delta;
  
  // Calculate the regressor effects for each of the six process parameters
  vector[N_events] t0_beta = X * beta_t0;
  vector[N_events] tp_beta = X * beta_tp;
  vector[N_events] ts_beta = X * beta_ts;
  vector[N_events] m1_beta = X * beta_m1;
  vector[N_events] m2_beta = X * beta_m2;
  vector[N_events] m3_beta = X * beta_m3;
  
  matrix[N_events, K] t0_ind;
  matrix[N_events, K] tp_ind;
  matrix[N_events, K] ts_ind;
  matrix[N_events, K] m1_ind;
  matrix[N_events, K] m2_ind;
  matrix[N_events, K] m3_ind;
  
  // Find titre type and n_event for this test
  for (i in 1:N) {
    int n_event = id[i];
    int k_index = titre_type[i];
  
    t0_ind[n_event, k_index] = t0_pop[k_index] + t0_beta[n_event] + sigma_t0_ind[k_index] * z_t0[n_event];
    tp_ind[n_event, k_index] = tp_pop[k_index] + tp_beta[n_event] + sigma_tp_ind[k_index] * z_tp[n_event];
    ts_ind[n_event, k_index] = ts_pop[k_index] + ts_beta[n_event] + sigma_ts_ind[k_index] * z_ts[n_event];
    m1_ind[n_event, k_index] = m1_pop[k_index] + m1_beta[n_event] + sigma_m1_ind[k_index] * z_m1[n_event];
    m2_ind[n_event, k_index] = m2_pop[k_index] + m2_beta[n_event] + sigma_m2_ind[k_index] * z_m2[n_event];
    m3_ind[n_event, k_index] = m3_pop[k_index] + m3_beta[n_event] + sigma_m3_ind[k_index] * z_m3[n_event];
  }
}

model {
  
  vector[N] mu;
  mu = boost_wane_ind(
    t, t0_ind, tp_ind, ts_ind, m1_ind, m2_ind, m3_ind, titre_type, id);
  
  // Likelihood for uncensored observations
  value[uncens_idx] ~ normal(mu[uncens_idx], sigma);

  // Likelihood for observations at lower limit
  target += normal_lcdf(lower_limit | mu[cens_lo_idx], sigma);

  // Censoring at upper limit
  target += normal_lccdf(upper_limit | mu[cens_hi_idx], sigma);
  
  // Covariate-level mean priors, parameterised from previous studies
  t0_pop ~ normal(mu_t0, sigma_t0);
  tp_pop ~ normal(mu_tp, sigma_tp);
  ts_pop_delta ~ normal(mu_ts - mu_tp, sigma_ts); // remember, this is defined relative to tp 
  m1_pop ~ normal(mu_m1, sigma_m1);
  m2_pop ~ normal(mu_m2, sigma_m2);
  m3_pop ~ normal(mu_m3, sigma_m3);
  
  // Covariate-level variation
  sigma_t0_ind ~ normal(0, preds_sd);
  sigma_tp_ind ~ normal(0, preds_sd*1.5);
  sigma_ts_ind ~ normal(0, preds_sd*2.5);
  sigma_m1_ind ~ normal(0, preds_sd);
  sigma_m2_ind ~ normal(0, preds_sd);
  sigma_m3_ind ~ normal(0, preds_sd);
  
  // Individual-level random effects (non-centered parameterisation)
  z_t0 ~ std_normal();
  z_tp ~ std_normal();
  z_ts ~ std_normal();
  z_m1 ~ std_normal();
  z_m2 ~ std_normal();
  z_m3 ~ std_normal();
  
  // Beta regression coefficients 
  beta_t0 ~ normal(0, preds_sd);
  beta_tp ~ normal(0, preds_sd*1.5);
  beta_ts ~ normal(0, preds_sd*2.5);
  beta_m1 ~ normal(0, preds_sd);
  beta_m2 ~ normal(0, preds_sd);
  beta_m3 ~ normal(0, preds_sd);

  // Overall measurement error
  sigma ~ normal(0, 2);
}
