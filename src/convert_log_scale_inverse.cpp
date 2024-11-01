#include <cpp11.hpp>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
cpp11::data_frame convert_log2_scale_inverse_cpp(cpp11::writable::list dt,
                                                 cpp11::strings vars_to_transform,
                                                 double lower_limit) {

  for (int i = 0; i < vars_to_transform.size(); i++) {
    std::string var = vars_to_transform[i];

    std::vector<double> col = cpp11::as_cpp<std::vector<double>>(dt[var]);
    
    // Apply the inverse transformation
    for (int j = 0; j < col.size(); j++) {
      col[j] = lower_limit * pow(2, col[j]);
    }
    
    // Replace the column
    dt[var] = cpp11::writable::doubles(col);
  }
  
  return cpp11::writable::data_frame(dt);
}
