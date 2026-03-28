
## 3.1

n <- 5000
lam <- 1
eta <- 1
x <- seq(0,1,0.01)
s <- r2exp(n, lam, eta)
hist(s, breaks = 50, freq = FALSE, xlim = c(0,10))
curve(d2exp(x, lam, eta), type = "l", add = TRUE)

sample_rexp <- quantile(s, probs = seq(0, 1, .1))
theoretical <- q2exp(seq(0,1,.1), 1, 1)
p <- seq(0, 1, .1)
se <- 1/ (lam * sqrt(n)) * sqrt(p) / sqrt(1-p)
rbind(sample_rexp, theoretical, se)

## 3.2

n <- 30000
u1 <- runif(n, 0, 1/2)
u2 <- runif(n,1/2, 1)
x1 <- log(2*u1)
x2 <- -log(2*(1-u2))
x <- c(x1,x2)
hist(x, breaks = 100, freq = FALSE, xlim = c(-10, 10))
curve(.5 * exp(-abs(x)), add = TRUE)

## 3.3

n <- 10000
u <- runif(n, 0, 1)
a <- 2
b <- 2
x <- b / ((1-u)^(1/a))
hist(x, breaks = 100, freq = FALSE, xlim = c(0,20))
curve(a * b^a * x^(-a-1), add = TRUE)

## 3.4


sig <- 1:4
par(mfcol = c(2,2))
for (i in 1:4){
  n <- 10000
  u <- runif(n, 0, 1)
  x <- sig[i]*sqrt(-log(u^2))
  hist(x, breaks = 40, freq = FALSE, xlim = c(1,10), ylim = c(0,.8))
  curve( (x/(sig[i]^2)) * exp(-x^2 / (2*sig[i]*sig[i]))  , add =  TRUE, 
         col = "red", lwd = 2)
}

## 3.5

n <- 1000
u <- runif(n, 0, 1)
sample <- proportions(table(findInterval(u, c(0, 0.1, 0.3, 0.5, 0.7, 1)) - 1))
given_pmf <- c(0.1, 0.2, 0.2, 0.2, 0.3)
rbind(sample, given_pmf)

## 3.6: skip cause it's a proof

## 3.7: acceptance rejection method

r.beta.AJ <- function(N, A, B){
  n <- N
  k <- 1
  y <- numeric(n)
  
  a <- A
  b <- B
  
  f <- function(x, a, b){
    y <- x^(a-1) * (1-x)^(b-1)
    B <- gamma(a) * gamma(b) / (gamma(a+b))
    return(y/B)
  }
  
  M <- 1.9
  
  while (k <= n){
    x <- runif(1) ## sampling from the proposed runif(1) when scaled by M meets req.
    u <- runif(1)
    
    if(u <= f(x, a, b)/M){
      y[k] <- x
      k <- k + 1
    }
  }
  return(y)
}

#r.beta.AJ(10, 2,2)

hist(y, breaks = 100, freq = FALSE)
curve(dbeta(x,3,2), add = TRUE, col = "red", lwd = 2)

## 3.8

n <- 4000
z <- rnorm(n, 0, 1)
mu <- 0
sig <- 1
x <- exp(mu + sig*z)

hist(x, breaks = 100, freq = FALSE)
curve(dlnorm(x, mu, sig), col = "red", lwd = 2, add = TRUE)

## 3.9

n <- 20000
x <- numeric(n)

for (i in 1:n){
  U <- runif(3, -1, 1)
  if (abs(U[3]) >= abs(U[2]) & abs(U[3]) >= abs(U[1])){
    x[i] <- U[2]
  }
  else{
    x[i] <- U[3]
  }
}

hist(x, breaks = 30, freq = FALSE, xlim = c(-1,1))
curve(3* (1-x^2)/4, add = TRUE, col = "red", lwd = 2)

## 3.10: skip the proof problems

## 3.11: mixture


par(mfcol = c(3,2))
for (p in seq(0.05, 0.95, 0.05)){
  p2 <- 1 - p
  n <- 2000
  x <- numeric(n)
  
  for (j in 1:n){
    z <- c(rnorm(1, 0, 1), rnorm(1, 3, 1))
    x[j] <- sample(z, size = 1, prob = c(p, p2))
  }
  hist(x, breaks = 50, main = paste("p1 = ",p), freq = FALSE)
}

## bimodality most pronounced for p1 in c(.35, .65)

## 3.12

n <- 3000
r <- 4
b <- 2
L <- rgamma(n, r, b)
Y <- numeric(n)

for (i in 1:n){
  Y[i] <- rexp(1, L[i])
}

hist(Y, breaks = 100, freq = FALSE)
curve( r* (b^r) / ((x+b)^(r+1)), col = "red", lwd = 2, 
       add =TRUE)

## 3.13

r.pareto <- function(N, R, beta){
  n <- N
  r <- R
  b <- beta
  L <- rgamma(n, r, b)
  Y <- numeric(n)
  
  for (i in 1:n){
    Y[i] <- rexp(1, L[i])
  }
  return(Y)
}

hist(r.pareto(10000, 4, 2), breaks = 50, freq = FALSE) ## kind of already did this in 3.12 
curve( r* (b^r) / ((x+b)^(r+1)), col = "red", lwd = 2, 
       add =TRUE)

## 3.14

rmvn.Choleski <- function(n, mu, Sigma){
  d <- length(mu)
  Q <- chol(Sigma)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z%*% Q + matrix(mu, n, d, byrow = TRUE)
  X
}

mu <- c(0,1,2)
Sig <- matrix(numeric(9), ncol = 3)

for (i in 1:3){
  for (j in 1:3){
    Sig[i, j] <- (-1)^(i+j) * 0.5
  }
}
Sig <- Sig + diag(.5, 3)

X <- rmvn.Choleski(200, mu, Sig)
pairs(X)

## 3.15: standardize a MVN


mu <- c(0,1,2)
Sig <- matrix(numeric(9), ncol = 3)

for (i in 1:3){
  for (j in 1:3){
    Sig[i, j] <- (-1)^(i+j) * 0.5
  }
}
Sig <- Sig + diag(.5, 3)
X <- mvrnorm(200, mu, Sig)


mvn_standardize <- function(X){ 
  X <- as.matrix(X)
  mu_hat0 <- colMeans(X)
  cov0 <- cov(X)
  
  # print(cov0)
  # print("prior sample mean vector is:") 
  # print(mu_hat0)
  
  ## centering
  Xc <- sweep(X, 2, mu_hat0)
  
  R <- chol(cov0)
  Z <- Xc %*% solve(R)

  # print(colMeans(Z))
  # print(cov(Z))
  return(Z)
}

mvn_standardize(X)

## 3.16: practice standardizing mvn

install.packages("bootstrap")
library(bootstrap)


closed_book <- scor[,1:2]
open_book <- scor[,3:5]

cb_stan <- mvn_standardize(closed_book)
ob_stan <- mvn_standardize(open_book)

# plot(cb_stan)
# scatterplot3d(ob_stan)

## 3.17: comparing performace of our makeshift rbeta function

n <- 5000
j <- 1000

system.time(
  for (i in 1:j){
    r.beta.AJ(N = n, A = 1, B = 2)
  }
)

system.time(
  for (i in 1:j){
    rbeta(n, 1, 2)
  }
)

## definitely getting outperformed by the built in rbeta function by a factor of 10^2

## 3.18: Generateing sample from W_{d}(\Sigma, n) for n > d+1 >=1

WishArt <- function(N, df, d, Sigma){
  wseq <- array(numeric(d*d*n), c(d,d, N))
  
  for (k in 1:N){
    t <- matrix(numeric(d^2), ncol = d)
    
    for (i in 1:d){
      for (j in 1:d){
        if (i > j) {
          t[i,j] <- rnorm(1, 0, 1)
        }
        else if (i == j){
          t[i,j] <- sqrt(rchisq(1, df = df - i + 1))
        }
      }
    }
    
    A <- t %*% t(t)
    R <- chol(Sigma)
    W1 <- t(R) %*% A %*% R
    wseq[,,k] <- W1
  }
  return(wseq)
}

W <- WishArt(10000, df = 2, d = 3, Sig)
round(apply(W, c(1,2), mean))

## end of chapter 3 exercises































