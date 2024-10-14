validate_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(paste0("'", name, "' must be logical"))
  }
}

validate_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(paste0("'", name, "' must be numeric"))
  }
}

validate_time_type <- function(time_type) {
  if (!(time_type %in% c("relative", "absolute"))) {
    stop("'time_type' must be one of 'relative' or 'absolute'")
  }
}

validate_formula <- function(covariate_formula) {
  if (!(class(covariate_formula) == "formula")) {
    stop("'covariate_formula' must be a formula")
  }
}

validate_formula_vars <- function(formula_vars, data) {
  unknown_vars <- formula_vars[which(!(formula_vars %in% names(data)))]
  if (length(unknown_vars) > 0) {
    stop(paste("All variables in 'covariate_formula' must correspond to data columns. Found unknown variables:",
               paste(unknown_vars, collapse = ", ")))
  }
}

validate_required_cols <- function(dat, required_cols = c("pid", "day", "last_exp_day", "titre_type", "value")) {
  missing_cols <- required_cols[!(required_cols %in% colnames(dat))]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }
}

validate_priors <- function(priors) {
  if (!inherits(priors, "biokinetics_priors")) {
    stop("'priors' must be of type 'biokinetics_priors'")
  }
}
