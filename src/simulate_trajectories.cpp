#include <cpp11.hpp>
#include <vector>
using namespace cpp11;

std::vector<double> simulate_trajectory_cpp(
    int t_max, double t0_ind, double tp_ind, double ts_ind, double m1_ind, double m2_ind, double m3_ind) {
  std::vector<double> mu(t_max + 1);
  for(int t = 0; t <= t_max; t++) {
    if(t <= tp_ind) {
      mu[t] = t0_ind + m1_ind * t;
    } else if(t <= ts_ind) {
      mu[t] = t0_ind + m1_ind * tp_ind + m2_ind * (t - tp_ind);
    } else {
      mu[t] = t0_ind + m1_ind * tp_ind + m2_ind * (ts_ind - tp_ind) + m3_ind * (t - ts_ind);
    }
    mu[t] = std::max(mu[t], 0.0);  // Ensure non-negative value
  }
  return mu;
}

[[cpp11::register]]
cpp11::writable::data_frame simulate_trajectories_cpp(const data_frame &person_params) {
  // Extract parameters from DataFrame
  integers stan_id = person_params["stan_id"];
  doubles t0_ind = person_params["t0_ind"];
  doubles tp_ind = person_params["tp_ind"];
  doubles ts_ind = person_params["ts_ind"];
  doubles m1_ind = person_params["m1_ind"];
  doubles m2_ind = person_params["m2_ind"];
  doubles m3_ind = person_params["m3_ind"];
  integers t_max_individual = person_params["t_max"];
  integers titre_type = person_params["k"];
  integers draw = person_params["draw"];

  // Create a placeholder for results
  std::vector<int> out_id, out_titre_type, out_draw, out_t;
  std::vector<double> out_mu;

  for(int p = 0; p < stan_id.size(); p++) {
    int curr_id = stan_id[p];
    int curr_titre_type = titre_type[p];
    int curr_draw = draw[p];
    int t_max = t_max_individual[p];

    std::vector<double> mu = simulate_trajectory_cpp(
      t_max, t0_ind[p], tp_ind[p], ts_ind[p], m1_ind[p], m2_ind[p], m3_ind[p]);

    for(int t = 0; t <= t_max; t++) {
      out_id.push_back(curr_id);
      out_titre_type.push_back(curr_titre_type);
      out_draw.push_back(curr_draw);
      out_t.push_back(t);
      out_mu.push_back(mu[t]);
    }
  }

  return cpp11::writable::data_frame({
                            "stan_id"_nm = out_id,
                            "k"_nm = out_titre_type,
                            "draw"_nm = out_draw,
                            "t"_nm = out_t,
                            "mu"_nm = out_mu
                           });
}
