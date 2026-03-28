
## theory heavy chapter

## example 2.1

x <- rgeom(100000, 0.2)
mean(x)

## example 2.2

x <- rpois(1000, 4)
mean(x)

## example 2.3

two_p_exp <- function(x, lam, eta){
  
  y <- lam*exp(-lam * (x-eta))
}

curve(two_p_exp(x, 1, 1))
curve(two_p_exp(x, 2, 1), add = TRUE)

## example 2.4 ?? unsure how to code

## example 2.5: likelihood and log-likelihood very important

install.packages("tolerance")
library(tolerance)

X <- r2exp(1000, 2, 2)
hist(X, freq = FALSE, breaks = 40)
lines(density(X), col = "red", lwd = 3)

ind <- function(x, eta){
  if (x >= eta){
    return(1)
  }
  else {
    return(0)
  }
}


samp2 <- r2exp(100, 1, 3)

min(samp1)
min(samp2)

log_likelihood <- function(x, lam, eta){
  if (any(x < eta)) {
    return(-Inf)
  }
  
  n <- length(x)
  
  ll <- n * log(lam) - lam * sum(x - eta)
  
  return(ll)
}

N=200
samp1 <- r2exp(100, 2, 2)

rmax <- 1/(mean(samp1)-min(samp1)) 
x <- seq(0.2,3, length.out = N)
y <- numeric(N)
for( i in 1:N){
  y[i] <- log_likelihood(samp1, x[i], min(samp1))
}
plot(x,y, "l")
abline(v = rmax, col = "red")

## example 2.6 we skipping

## example 2.7: Markov Matrices

a <- runif(1, 0, 1/3)
a <- 0.1
A <- diag(1-4*a, 4) + matrix(a, 4, 4)

expm <- function(A, k){
  if (k == 1){
    return(A)
  }
  for (i in 2:k){
    A <- A %*% A
  }
  return(A)
}

expm(A, 4)
expm(A, 5)

## example 2.8

library(ggplot2)
random_walk <- function(num_walkers = 100, num_steps = 40){
  
  f_pos <- numeric(num_walkers)
  for (i in 1:num_walkers){
    steps <- sample(c(1,-1), size = num_steps, replace = TRUE)
    f_pos[i] <- 0 + sum(steps)
  }
  
  df <- data.frame(x = f_pos)
  y_height <- max(table(f_pos))
                  
  ggplot(df, aes(x = x)) +
    geom_dotplot(binwidth = 0.2, fill = "blue") +
    scale_x_continuous(
      limits = c(min(df$x) - 1, max(df$x) + 1),
      breaks = seq(min(df$x), max(df$x), by = 2)
    ) +
    scale_y_continuous(
      limits = c(0, y_height + 3),
      breaks = seq(0, y_height + 3, by = 1)
    ) +
    labs(
      title = "Dot Plot",
      x = paste("Final Position after", num_steps, "steps"),
      y = "Stack Height"
    )
}
random_walk(4000, 100)






