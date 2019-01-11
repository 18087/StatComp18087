#' @title Generate random samples from Cauchy distribution by MH sampler
#' @description Generate random samples from Cauchy distribution by MH sampler
#' @param n number of observations. 
#' @param theta location parameter
#' @param eta scale parameter
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rcauchy(100)
#' }
#' @export
rcauchy.MH <- function(n,theta=0,eta=1){
  x <- numeric(n)
  u <- runif(n)
  x[1] <- rnorm(1)
  k <- 0
  # cauchy functions
  f <- function(x, theta=1, eta=0){
    out <- 1/(pi * theta * (1+((x-eta)/theta)^2))
    return(out)
  }
  
  for(i in 2:n){
    xt <- x[i-1]
    y <- rnorm(1,mean=xt)
    R <- f(y)*dnorm(xt,mean=y)/(f(xt)*dnorm(y,mean=xt))
    if(u[i] <= R){
      x[i] <- y
    }else{
      x[i] <- xt
      k <- k+1
    }
  }
  
}



