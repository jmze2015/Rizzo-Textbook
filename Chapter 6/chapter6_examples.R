
## example 6.1

## estimating \int_0^1 e^{-x} dx using Monte Carlo Estimator


m <- 1000
E <- sum(exp(-1 * runif(m, 0, 1))) / m
A <- 1-(1/exp(1))
A-E

## example 6.2

m <- 1000
theta.hat <- mean(exp(-1 * runif(m, 2, 4))) * 2
theta <- exp(-2)-exp(-4)
print(theta.hat)
print(theta)

## example 6.3 (finite case of integral) 

m <- 5000000 
mean(exp(-1*(runif(m, -2, 2)^2) / 2) / sqrt(2*pi)) * 4

normCDF <- function(a,b){
  if (a < b){
    MCE <- mean(exp(-1*(runif(m, a, b)^2) / 2) / sqrt(2*pi)) * (b-a)
    return(MCE)
  }
  else break
  
}
normCDF(-10,1)
