#' Log-Likelihood Function for the Binomial Distribution
#'
#' Computes the log-likelihood for a binomial random variable with a fixed size of 10
#' and a given probability parameter.
#'
#' @param x An integer or numeric vector of observed values (number of successes).
#' @param param A numeric value representing the probability of success (between 0 and 1).
#'
#' @return A numeric vector of log-likelihood values.
#'
#' @export
logbin <- function(x, param) {
  log(dbinom(x, prob = param, size = 10))
}
