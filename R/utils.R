convert_log2_scale <- function(
  dt_in, vars_to_transform = "value",
  simplify_limits = TRUE) {

  dt_out <- data.table::copy(dt_in)
  for (var in vars_to_transform) {
    if (simplify_limits == TRUE) {
      dt_out[get(var) > 2560, (var) := 2560]
    }
    dt_out[, (var) := log2(get(var) / 5)]
  }
  return(dt_out)
}

#' @title Invert base 2 log scale conversion
#'
#' @description Natural scale data is converted to a base 2 log scale before model fitting. This
#' function reverses that transformation and may be useful if working directly with fitted parameters.
#' This function does not modify the provided data.table in-place, but returns a transformed copy.
#' @return A data.table, identical to the input data but with specified columns transformed.
#' @param dt_in data.table containing data to be transformed from base 2 log to natural scale.
#' @param vars_to_transform Names of columns to apply the transformation to.
#' @export
convert_log2_scale_inverse <- function(dt_in, vars_to_transform) {
  dt_out <- data.table::copy(dt_in)
  for (var in vars_to_transform) {
    # Reverse the log2 transformation and multiplication by 5.
    dt_out[, (var) := 5 * 2^(get(var))]
  }
  dt_out
}

#' @title Combine titre data and infection data into biokinetics input format.
#'
#' @description The biokinetics class requires a single data table with titre readings
#' and last exposure times for all individuals. If you have exposure times and titre readings for the same
#' set of individuals in separate files, this function will combine them into a single data.table.
#' @return A data.table with all columns required by the biokinetics model.
#' @param dat_sero data.table containing titre values in the format required by biokinetics. See data vignette: \code{vignette("data", package = "epikinetics")}.
#' @param dat_inf data.table containing exposure days and person ids corresponding to those in dat_sero. By default the exposure days are expected in a column called 'day'.
#' @param exposure_column Default 'day'. The name of the column containing exposure days. These can be integers or dates.
#' @export
add_exposure_data <- function(dat_sero, dat_inf, exposure_column = 'day') {
  validate_required_cols(dat_sero, required_cols = c("pid", "day", "titre_type", "value"))
  validate_required_cols(dat_inf, required_cols = c("pid", exposure_column))
  dat_inf[, "last_exp_day" := max(get(exposure_column)), by  = "pid"]
  merge(dat_sero, dat_inf[, c("pid", "last_exp_day")], by = "pid", allow.cartesian=TRUE)
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

build_covariate_lookup_table <- function(data, design_matrix, all_formula_vars) {

  p_name <- NULL

  if (length(all_formula_vars) == 0) {
    return(NULL)
  }
  p_names <- colnames(design_matrix)
  p_names <- data.table::data.table(p_name = p_names, p = seq_along(p_names))

  factors <- lapply(all_formula_vars, function(v) { levels(as.factor(as.data.frame(data)[, v])) })
  names(factors) <- all_formula_vars
  combinations <- expand.grid(factors)

  for (i in 1:nrow(combinations)) {
    combinations[i, "p_name"] <- paste0(all_formula_vars, as.matrix(combinations[i, all_formula_vars]), collapse = ":")
  }

  dt_out <- data.table::setDT(combinations)[p_names, on = "p_name"]

  for (f in names(factors)) {
    re <- paste0("(?<=", f, ").+?(?=$|:)")
    dt_out[, f] <- dt_out[, stringr::str_extract(p_name, re)]
  }

  dt_out[, p_name := NULL]
  dt_out
}

build_pid_lookup <- function(data) {
  pids <- unique(data$pid)
  ids <- seq_along(pids)
  pid_lookup <- ids
  names(pid_lookup) <- pids
  pid_lookup
}

build_titre_type_lookup <- function(data) {
  titre_types <- unique(data$titre_type)
  titre_type_nums <- seq_along(titre_types)
  titre_type_lookup <- titre_type_nums
  names(titre_type_lookup) <- titre_types
  titre_type_lookup
}
