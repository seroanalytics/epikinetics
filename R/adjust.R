adjust_parameters <- function(
    dt, params_to_adjust = c(
      "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop")) {
  
  dt_out <- copy(dt)
  
  # Loop through the parameters you want to adjust
  for (param in params_to_adjust) {
    # Remove the '_pop' suffix to construct the beta variable name
    beta_var <- paste0("beta_", gsub("_pop$", "", param))
    dt_out[, (param) := get(param) + get(beta_var)]
  }
  
  return(dt_out)
}
