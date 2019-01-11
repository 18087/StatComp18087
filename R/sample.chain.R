#' @title Gnerate Metropolis chain from giving multinomial distribution
#' @description Gnerate Metropolis chain  from giving multinomial distribution
#' @param N the length of chain
#' @param start the starting value
#' @return a random sample of size \code{N}
#' @examples
#' \dontrun{
#' rnR <- sample.chain(100,1)
#' plot(rnR,type='l')
#' }
#' @export
sample.chain <- function(N, start){
  #generates a Metropolis chain for multinomial distribution
  #with random walk proposal distribution and starting value start
  sizes <- c(125,18,20,34)
  x <- rep(0, N)
  x[1] <- start
  u <- runif(N)
  v <- runif(N,-0.25,0.25)
  prob <- function(theta){
    p <- c(0.5+theta/4,(1-theta)/4,(1-theta)/4,theta/4)
  }#the probabilities of the corresponding multinomial distribution
  prob.ratio <- function(x1,x2){
    prod(prob(x1)^sizes/prob(x2)^sizes)
  }

  for (i in 2:N){
    xt <- x[i-1]
    y <- xt + v[i] #candidate point
    r <- min(prob.ratio(y,xt),1)
    if (!is.nan(r) && u[i] <= r){
      x[i] <- y
    }else{
      x[i] <- xt
    }
  }
  return(x)
}
