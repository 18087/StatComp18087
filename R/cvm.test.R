#' @title The two-sample Cramer-von Mises test using R
#' @description The two-sample Cramer-von Mises test using R
#' @param x The sample from distribution X
#' @param y The sample from distribution Y
#' @return score of the two-sample Cramer-von Mises test for given samples and p value
#' @examples
#' \dontrun{
#' setseed(1121)
#' x <-  rpois(30,2)
#' y <-  rpois(30,3)
#' cvm.test(x,y)
#' }
#' @export
cvm.test <- function(x,y){
  r <- 1000 #permutation samples
  reps <- numeric(r) #score of the two-sample Cramer-von Mises test
  n <- length(x)
  m <- length(y)
  v.n <- numeric(n)
  v1.n <- numeric(n)
  v.m <- numeric(m)
  v1.m <- numeric(m)
  z <- c(x,y) #the combined sample
  N <- length(z)
  Ix <- seq(1:n)
  Iy <- seq(1:m)
  v.n <- (x-Ix)**2
  v.m <- (y-Iy)**2
  #test statistic
  reps_0 <- ((n * sum(v.n)+m * sum(v.m))/(m * n * N))-(4 * m * n - 1)/(6 * N)
  for (k in 1:r){#permutation samples
    w <- sample(N,size=n,replace=FALSE)
    x1 <- sort(z[w])
    y1 <- sort(z[-w])
    v1.n <- (x1-Ix)**2
    v1.m <- (y1-Iy)**2
    reps[k] <- ((n * sum(v1.n)+m * sum(v1.m))/(m * n * N))-(4 * m * n - 1)/(6 * N)
    }
  p <- mean(c(reps_0,reps) >= reps_0)
  res <- list(reps=reps,reps0=reps_0,pvalue=p)
  return(res)
}
