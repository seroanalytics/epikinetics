validate_time_type <- function(time_type) {
  if (!(time_type %in% c("relative", "absolute"))) {
    stop("'time_type' must be one of 'relative' or 'absolute'")
  }
}

validate_scale <- function(scale) {
  if (!(scale %in% c("natural", "log"))) {
    stop("'scale' must be one of 'natural' or 'log'")
  }
}

validate_covariates <- function(vec, formula) {
  all_vars <- all.vars(formula)
  res <- vec[!sapply(vec, function(v) v %in% all_vars)]
  if (length(res) > 0) {
    stop(paste0("'by' must contain variables present in hierarchical model. '",
                paste0(res, collapse = ", "), "' not present in model."))
  }
}
