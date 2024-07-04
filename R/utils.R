convert_log_scale_inverse <- function(dt_in, vars_to_transform) {

  for(var in vars_to_transform) {
    # # Reverse the log2 transformation and multiplication by 5.
    dt_in[, (var) := 5*2^(get(var))]

  }
  return(dt_in)
}

summarise_draws <- function(dt_in, column_name, by = by) {
  # Declare variables to suppress notes when compiling package
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  . <- NULL

  dt_out <- dt_in[, .(
    me = stats::quantile(get(column_name), 0.5),
    lo = stats::quantile(get(column_name), 0.025),
    hi = stats::quantile(get(column_name), 0.975)
  ),
                    by = by
  ]

  return(dt_out)
}
