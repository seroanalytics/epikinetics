#' @title Construct priors for the biomarker model.
#' @export
#' @description The biokinetics model has 6 parameters: t0, tp, ts, m1, m2, m3 corresponding to critical time points and
#' gradients. See the model vignette for details: \code{vignette("model", package = "epikinetics")}. Each of these
#' parameters has a Gaussian prior, and these can be specified by the user. This function takes means and standard
#' deviations for each prior and constructs an object of type 'biokinetics_priors' to be passed to the model.
#' @return A named list of type 'biokinetics_priors'.
#' @param mu_t0 Numeric. Mean for t0, baseline titre value. Default 4.0.
#' @param mu_tp Numeric. Mean for tp, time at peak titre. Default 10.
#' @param mu_ts Numeric. Mean for ts, time at start of warning. Default 60.
#' @param mu_m1 Numeric. Mean for m1, boosting rate. Default 0.25.
#' @param mu_m2 Numeric. Mean for m2, plateau rate. Default 0.25.
#' @param mu_m3 Numeric. Mean for m3, waning rate. Default -0.02.
#' @param sigma_t0 Numeric. Standard deviation for t0, baseline titre value. Default 2.0.
#' @param sigma_tp Numeric. Standard deviation for tp, time at peak titre. Default 2.0.
#' @param sigma_ts Numeric. Standard deviation for ts, time at start of warning. Default 3.0.
#' @param sigma_m1 Numeric. Standard deviation for m1, boosting rate. Default 0.01.
#' @param sigma_m2 Numeric. Standard deviation for m2, plateau rate. Default 0.01.
#' @param sigma_m3 Numeric. Standard deviation for m3, waning rate. Default 0.01.
#' @examples
#' priors <- biokinetics_priors(mu_t0 = 5.0, mu_ts = 61)
biokinetics_priors <- function(mu_t0 = 4.0, mu_tp = 10, mu_ts = 60, mu_m1 = 0.25, mu_m2 = -0.02, mu_m3 = 0,
                               sigma_t0 = 2.0, sigma_tp = 2.0, sigma_ts = 3.0, sigma_m1 = 0.01, sigma_m2 = 0.01, sigma_m3 = 0.01) {
  ret <- as.list(environment())
  class(ret) <- append("biokinetics_priors", class(ret))
  ret
}
