gaussian_priors <- function(names, mu_values, sigma_values) {
  if (length(names) != length(mu_values) ||
    length(names) != length(sigma_values)) {
    stop("The lengths of the vectors do not match.")
  }
  mu_names <- paste0("mu_", names)
  sigma_names <- paste0("sigma_", names)
  ret <- as.list(c(mu_values, sigma_values))
  names(ret) <- c(mu_names, sigma_names)
  class(ret) <- append("gaussian_priors", class(ret))
  ret
}

#' @title Construct priors for the biomarker model.
#' @export
#' @description The biokinetics model has 6 parameters: t0, tp, ts, m1, m2, m3 corresponding to critical time points and
#' gradients. See the model vignette for details: \code{vignette("model", package = "epikinetics")}. Each of these
#' parameters has a Gaussian prior, and these can be specified by the user. This function takes means and standard
#' deviations for each prior and constructs an object of type 'biokinetics_priors' to be passed to the model.
#' @return A named list of type 'biokinetics_priors'.
#' @param mu_values Mean of Gaussian prior for each of t0, tp, ts, m1, m2, m3, in order.
#' @param sigma_values Standard deviation of Gaussian prior for each of t0, tp, ts, m1, m2, m3, in order.
#' @examples
#' priors <- biokinetics_priors(mu_values = c(4.0, 10, 60, 0.25, -0.02, 0),
#' sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.01))
biokinetics_priors <- function(mu_values = c(4.0, 10, 60, 0.25, -0.02, 0),
                         sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.01)) {
  names <- c("t0", "tp", "ts", "m1", "m2", "m3")
  ret <- gaussian_priors(names, mu_values, sigma_values)
  class(ret) <- append("biokinetics_priors", class(ret))
  ret
}
