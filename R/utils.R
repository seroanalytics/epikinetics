convert_log_scale <- function(
  dt_in, vars_to_transform = "titre",
  simplify_limits = TRUE) {

  dt_out <- data.table::copy(dt_in)
  for(var in vars_to_transform) {
    if(simplify_limits == TRUE) {
      dt_out[get(var) > 2560, (var) := 2560]
    }
    dt_out[, (var) := log2(get(var)/5)]
  }
  return(dt_out)
}

#' @title Invert base 2 log scale conversion
#'
#' @description User provided data is converted to a base 2 log scale before model fitting. This
#' function reverses that transformation. This function does not modify the provided data.table in-place,
#' but returns a transformed copy.
#' @return A data.table, identical to the input data but with specified columns transformed.
#' @param dt_in data.table containing data to be transformed from base 2 log to natural scale.
#' @param vars_to_transform Names of columns to apply the transformation to.
#' @export
convert_log_scale_inverse <- function(dt_in, vars_to_transform) {
  dt_out <- data.table::copy(dt_in)
  for(var in vars_to_transform) {
    # Reverse the log2 transformation and multiplication by 5.
    dt_out[, (var) := 5*2^(get(var))]
  }
  dt_out
}

summarise_draws <- function(dt_in, column_name, by = by) {
  # Declare variables to suppress notes when compiling package
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  . <- NULL

  dt_in[, .(
    me = stats::quantile(get(column_name), 0.5),
    lo = stats::quantile(get(column_name), 0.025),
    hi = stats::quantile(get(column_name), 0.975)
  ),
                    by = by
  ]
}
