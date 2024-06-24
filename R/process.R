recover_covariate_names <- function(
  dt, dt_stan_inputs, formula) {

  dt_titre_lookup <- data.table(
    k = 1:dt_stan_inputs[, length(unique(titre_type))],
    titre_type = dt_stan_inputs[, unique(titre_type)])

  dt_covariate_lookup <- covariate_lookup_table(
    dt_stan_inputs, formula)

  dt[
    dt_covariate_lookup, on = "p"][
    dt_titre_lookup, on = "k"]
}

covariate_lookup_table <- function(data, covariate_formula) {
  # Extract column names
  col_names <- colnames(construct_design_matrix(data, covariate_formula))

  # Split column names based on the ':' delimiter
  split_data <- stringr::str_split(col_names, ":", simplify = TRUE)

  # Convert the matrix to a data.table
  dt <- as.data.table(split_data)

  # Extract category names from formula
  formula_vars <- all.vars(covariate_formula)

  # Set the new column names
  setnames(dt, formula_vars)

  for (col_name in names(dt)) {
    # Find the matching formula variable for current column
    matching_formula_var <- formula_vars[which(startsWith(col_name, formula_vars))]
    if (length(matching_formula_var) > 0) {
      pattern_to_remove <- paste0("^", matching_formula_var)
      dt[, (col_name) := stringr::str_remove_all(get(col_name), pattern_to_remove)]
    }
  }

  # .I is a special symbol in data.table for row number
  dt[, p := .I]

  # Reorder columns to have 'i' first
  setcolorder(dt, "p")

  return(dt)
}
