#' Maximum Likelihood Plotter for 1D Likelihood Functions
#'
#' Evaluates a log-likelihood function over a range of parameter values and plots the likelihood curve (exponentiated log-likelihood).
#' The function highlights the parameter value that maximizes the likelihood.
#'
#' @param lfun A log-likelihood function that takes a single numeric parameter as input.
#'             The default is `"logbin2"` which must be defined in the environment.
#' @param theta A numeric vector of parameter values to evaluate the likelihood over.
#'
#' @return The value of `theta` that maximizes the likelihood.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Evaluates the log-likelihood function at each value of `theta`.
#'   \item Converts the log-likelihood to standard likelihood via exponentiation.
#'   \item Plots the likelihood curve and adds a vertical line at the maximum.
#'   \item Displays the value of `theta` that gives the highest likelihood.
#' }
#'
#' @examples
#' # Example with a user-defined binomial log-likelihood function
#' logbin2 <- function(p) dbinom(5, size = 10, prob = p, log = TRUE)
#' mymaxlikg(lfun = logbin2, theta = seq(0.01, 0.99, length.out = 100))
#'
#' @export
mymaxlikg <- function(lfun = "logbin2", theta) {
  nth <- length(theta)
  thmat <- matrix(theta, nr = nth, nc = 1, byrow = TRUE)

  # Evaluate the log-likelihood function at each value
  z <- apply(thmat, 1, match.fun(lfun))

  # Find index of the maximum likelihood
  zmax <- max(which(z == max(z)))

  # Plot the exponentiated log-likelihood (i.e., the likelihood)
  plot(theta, exp(z), type = "l", xlab = "Parameter", ylab = "Likelihood",
       main = "Likelihood Function")
  abline(v = theta[zmax], col = "blue")
  axis(3, theta[zmax], round(theta[zmax], 4))

  # Return the parameter value that maximizes the likelihood
  return(theta[zmax])
}

