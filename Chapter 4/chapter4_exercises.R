
## 4.1 

reps <- 10000
Sn <- numeric(reps)

## many simulations to answer average time to hit a barrier

for (i in 1:reps){
  A <- 10
  n <- 0
  while (A < 20 & A > 0){
    jump <- sample(c(-1, 1), size = 1)
    A <- A + jump
    n <- n + 1
  }
  Sn[i] <- n
}

mean(Sn)

## for a single path we do the following instead

A <- c(10)

i <- 1
while(A[i] != 0 & A[i] != 20){
  jump <- sample(c(-1, 1), size = 1)
  A <- c(A, A[i] + jump)
  i <- i + 1
}

t <- seq(1, length(A))
plot(t, A, type = "l", main = "Simulated Fortune w/ Barriers at $0 and $20", ylim = c(-1.5, 20.5))
abline(h = c(0, 20), lty = 2, col = "red")

## 4.2

reps <- 100000
Xt <- numeric(reps)
for (i in 1:reps){
  t <- 10
  lambda <- 3
  a <- 2
  b <- 2
  Nt <- rpois(1, lambda = lambda * t)
  Y <- rgamma(Nt, a, b) # gamma distributed jumps
  Xt[i] <- sum(Y) 
}

mean(Xt) ## ~30 = lambda * t * E[Y] (E[Y]= a/b)
var(Xt)

## 4.3

## lam(t) = 2*t + 2 set t0 = 5 then the upper bound lam_0 = 12.

exp_arrival <- numeric(1000)
for (i in 1:1000){
  t0 <- 5
  lambda_0 <- 12
  upper <- 100
  N <- rpois(1, lambda_0 * upper)
  Tn <- rexp(N, lambda_0)
  Sn <- cumsum(Tn)
  Un <- runif(N)
  keep <- (Un <= (2*Sn + 2)/lambda_0 )
  
  arrival <- sum(Sn[keep] <= t0 & Sn[keep] >= 4)
  exp_arrival[i] <- arrival 
}

mean(exp_arrival)












