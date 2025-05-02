
#' computes the area of curve
#'
#' @param mu mean
#' @param sigma standard devetion
#' @param doh value up to which the area under the curve
#' @returns list of mean and sd and can be made into a curve
#'
#' @export
myncurve = function(mu, sigma,doh){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(mu - 3*sigma, doh, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)

  polygon(c(mu - 3*sigma, xcurve, doh), c(0, ycurve, 0), col="orange")

  area = pnorm(doh, mean=mu, sd=sigma)
  area = round(area, 4)
  text(x = mu - 2*sigma, y = max(ycurve)/2, paste("Area =", area), col="purple")
  return (list(mu = mu, sigma = sigma, area = area))
}

