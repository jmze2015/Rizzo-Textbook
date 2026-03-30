
## example 4.1: Poisson process

N3 <- numeric(100)
for (i in 1:100){
  n <- 1000
  lambda <- 2
  t0 <- 3
  Tn <- rexp(n, lambda)
  Sn <-cumsum(Tn)
  n <- min(which(Sn > t0))
  
  
  N3[i] <- n-1
  
}
mean(N3)

## example 4.2

lambda <- 2
t0 <- 3
upper <- 100
pp <- numeric(10000)

for (i in 1:10000){
  N <- rpois(1, lambda * upper) 
  Sn <- sort(runif(N, 0 , upper)) ## ordered arrivals times
  n <- min(which(Sn > t0))
  pp[i] <- n-1
}



c(mean(pp), var(pp)) ## expecting lambda * t0 for mean and var

## example 4.3: simulate NHPP using lam(t)= 3cos^{2}(t).

lambda <- 3
upper <- 100
N <- rpois(1, lambda * upper)
Tn <- rexp(N, lambda)
Sn <- cumsum(Tn)
Un <- runif(N)
keep <- (Un <= cos(Sn)^2)
Sn[keep]
sum(Sn[keep] <= 2*pi)
table(keep)/N

## example 4.4: Simulating renewal process where interarrival times ~Geom(p)

## one iteration
t0 <- 5
Tn <- rgeom(100, prob = .2) ## interarrival times (simulated time between events)
Sn <- cumsum(Tn) ## honest to god arrival times 
Sn
n <- min(which(Sn > t0))
n-1

## many iterations

rp <- numeric(1000)
for (i in 1:1000){
  t0 <- 5
  Tn <- rgeom(100, prob = .2) ## interarrival times (simulated time between events)
  Sn <- cumsum(Tn) ## honest to god arrival times 
  n <- min(which(Sn > t0))
  rp[i] <- n-1
}
mean(rp)

## method 2: using replicate function

Nt0 <- replicate(1000, expr ={
  Sn <- cumsum(rgeom(100, prob = .2))
  min(which(Sn > t0)) - 1
})
#mean(Nt0)
table(Nt0)/1000

## now vary the time t0 i.e. the interval [0, t0]

t0 <- seq(0.1, 30, 0.1)
mt <- numeric(length(t0))

for (i in 1:length(t0)){
    mt[i] <- mean(replicate(1000, expr = {
    Sn <- cumsum(rgeom(100, prob = 0.2))
    min(which(Sn > t0[i])) - 1
  }))
}


plot(t0, mt, type = "l", xlab = "t", ylab = "mean")
abline(0, 0.25, lty = 2)

## example 4.5: partial realization of a random walk

n_steps <- 500
pos <- cumsum(sample(c(-1,1), size = n_steps, replace = TRUE))
which(pos == 0)
plot(seq(1,n_steps), pos, type = "l", xlab = "Number of Steps", 
     main = "Random Walk")
grid()

## example 4.6: Algorithm to generate Sn of a symmetric walk

n <- 1:10000
P2n <- (.5/n) * dbinom(n-1, size = 2*n -2, prob =0.5)
pP2n <- cumsum(P2n)
u <- runif(1)
Tj <- 2 * (1+sum(u >pP2n))
pP2n

## the following simulates t0 the time of last return to 0 within [0, n]
n <- 200

P2n <- (.5/n) * dbinom(n-1, size = 2*n -2, prob =0.5)
pP2n <- cumsum(P2n)
u <- runif(1)
Tj <- 2 * (1+sum(u >pP2n))

sumT  <- 0
while(sumT <= n){
  u <- runif(1)
  s <- sum(u > pP2n)
  if (s == length(pP2n)) warning("T is truncated")
  print(c(Tj, sumT))
  Tj <- 2 * (1+s)
  
  sumT <- sumT + Tj
}
sumT -Tj

## example 4.7

simBM <- function(n, t, sims = 10){
  times <- seq(0, t, length = n)
  W <- matrix(numeric(n * sims), ncol = sims )
  
  for (i in 1:sims){
    for (k in 1:(n-1)){
      Zk <- rnorm(1)
      sdk <- sqrt(times[k+1] - times[k])
      W[k+1, i] <- W[k, i] + sdk * Zk
    }
  }
  return(list(
    t = times, w = W
  ))
}

set.seed(1)

A <- simBM(100, 3, 80)

plot_simBM <- function(A){
  r <- range(A$w)
  d <- ncol(A$w)
  n <- length(A$t)
  plot(A$w[, 1], type = "l", main = paste("1-D Brownian Motion Simulation (",d," walkers, t = ", A$t[n], " secs )"), 
       xlab = "Number of Steps", ylab = "Position", ylim = r)
  for (j in 2:d){
    lines(A$w[, j], lty = 2)
  }
}

plot_simBM(A) ## would be very cool to overlay a 3d normal curve above this

abline(a = 0, b = 2*sqrt(3)/100, col = "red")
abline(a = 0, b = -2*sqrt(3)/100, col = "red")
abline(a = 0, b = sqrt(3)/100, col = "blue")
abline(a = 0, b = -sqrt(3)/100, col = "blue")

lines(density(A$w[100,]))

## 2d Brownian Motion Simulation

install.packages("gganimate")
install.packages("gifski")
install.packages("transformr")

library(ggplot2)
library(gganimate)

B <- simBM(200, t = 5, sims = 2)
B_df <- as.data.frame(B)
plot_simBM(B)
p <- ggplot(B_df, aes(x = w.1, y = w.2)) +
  geom_path(color = "blue") +
  geom_point(size = 2, color = "red") +
  transition_reveal(t) +
  labs(title = "Time: {frame_along}")

## animate(p, fps = 20, width = 600, height = 600) 

#library(plotly)

plot_ly(
  B_df,
  x = ~w.1,
  y = ~w.2,
  frame = ~t,
  type = 'scatter',
  mode = 'lines+markers'
)


















