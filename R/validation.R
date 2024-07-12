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
