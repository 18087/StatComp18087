#' @title Generate samples from Rayleigh distribution
#' @description Generate samples from Rayleigh distribution
#' @param x vector of quantiles
#' @param sigma the parameter of Rayleigh density
#' @param m the sample size
#' @param antithetic using antithetic method or not
#' @return a random sample of size \code{length(x)}
#' @examples
#' \dontrun{
#' rnR <- sample.chain(100,1)
#' plot(rnR,type='l')
#' }
#' @export
Rayleigh.MC <- function(x, sigma, m = 10000, antithetic = TRUE) {
  u <- runif(m/2)
  if (!antithetic){
    v <- runif(m/2)
  }else{
    v <- 1 - u
  }
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i]^2/sigma^2 * u * exp(-x[i]^2/(2 * sigma^2) * u^2)
    cdf[i] <- mean(g)
  }
  return(cdf)
}
