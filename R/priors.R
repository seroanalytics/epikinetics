#' @title Construct Gaussian priors for model variables.
#' @export
#' @description Construct an object of type 'epikinetics_priors' to pass to the model.
#' @return A named list of type 'epikinetics_priors'.
#' @param names Names of the variables.
#' @param mu_values Mean of Gaussian prior for each named variable.
#' @param sigma_values Standard deviation of Gaussian prior for each named variable.
#' @examples
#' priors <- epikinetics_priors(names = c("t0", "tp", "ts", "m1", "m2", "m3"),
#' mu_values = c(4.0, 10, 60, 0.25, -0.02, 0), sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.01))
epikinetics_priors <- function(names = c("t0", "tp", "ts", "m1", "m2", "m3"),
                               mu_values = c(4.0, 10, 60, 0.25, -0.02, 0),
                               sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.01)) {
  if (length(names) != length(mu_values) ||
    length(names) != length(sigma_values)) {
    stop("The lengths of the vectors do not match.")
  }
  mu_names <- paste0("mu_", names)
  sigma_names <- paste0("sigma_", names)
  ret <- as.list(c(mu_values, sigma_values))
  names(ret) <- c(mu_names, sigma_names)
  class(ret) <- "epikinetics_priors"
  ret
}
