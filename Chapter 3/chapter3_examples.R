
## example 3.1

## toss some coins
sample(0:1, size = 10, replace = TRUE)

## pick lottery numbers
sample(1:100, size = 6, replace = TRUE)

## permutation of alph
sample(letters)

x <- sample(1:3, size =1000, replace = TRUE, prob = c(0.2, 0.3, 0.5))
proportions(table(x))

## example 3.2: The Inverse Transform Method (continuous case)

# take pdf 3x^2 -> cdf: x^3 -> inverse: u^(1/3) if u ~U[0,1] 
# then X=F^{-1}_{X}(u) will follow the pdf
n <- 1000
u <- runif(n)
x <- u^(1/3)
hist(x, prob = TRUE, main=expression(f(x)==3*x^2))
y <- seq(0,1,0.1)
lines(y, 3*y^2)

## example 3.3

n <- 1000
u <- runif(n)
lam <- 2
x <- -(1/lam)*log(1-u)
hist(x, prob = TRUE, breaks = 25)
y <- seq(0, 5, 0.1)
lines(y, lam* exp(-lam * y))

## example 3.4: Inverse method (discrete case)

n <- 1000
p <- 0.4
u <- runif(n)
x <- as.integer(u > 0.6)

mean(x)
var(x)

## example 3.5: Inverse method w/ geometric distribution

n <- 1000
u <- runif(n)
p <- 0.25
k <- ceiling(log(1-u) / log(1-p))-1
hist(k, freq = FALSE)

## example 3.6

rlogarithmic <- function(n, theta){
  u <- runif(n)
  
  N <- ceiling(-16 / log10(theta))
  k <- 1:N
  a <- -1 / log(1 - theta)
  
  fk <- exp(log(a) + k * log(theta) - log(k))
  Fk <- cumsum(fk)
  
  x <- integer(n)
  
  for (i in 1:n){
    x[i] <- sum(u[i] > Fk)
    
    while (x[i] == N){
      logf <- log(a) + (N + 1) * log(theta) - log(N + 1)
      fk <- c(fk, exp(logf))
      Fk <- c(Fk, tail(Fk, 1) + fk[length(fk)])
      N <- N + 1
      
      x[i] <- sum(u[i] > Fk)
    }
  }
  
  return(x + 1)
}

n <- 1000
theta <- 0.5
x <- rlogarithmic(n, theta)
k <- sort(unique(x))
p <- -1 / log(1-theta) * theta^k / k
se <- sqrt(p*(1-p)/n)
round(rbind(table(x)/n, p, se),3)

## example 3.7: Acceptance Rejection Method

n <- 10000
k <- 0 # counter for accepted
j <- 0 # iterations
y <- numeric(n)

while( k < n){
  u <- runif(1)
  j <- j+1
  x <- runif(1)
  if (x * (1-x) > u){
    k <- k + 1
    y[k] <- x
  }
}

j
p <- seq(.1, .9, .1)
Qhat <- quantile(y, p) # quantiles of samples
Q <- qbeta(p, 2, 2) # theoretical quantiles
se <- sqrt(p*(1-p) / (n*dbeta(Q, 2, 2)^2))

round(rbind(Qhat, Q, se), 3)

## example 3.8

n <- 1000
a <- 3
b <- 2
u <- rgamma(n, shape = a, rate = 1)
v <- rgamma(n, b, 1)
x <- u / (u+v)

q <- qbeta(ppoints(n), a, b)
qqplot(q, x, cex = 0.25, xlab = "beta(3,2)", ylab = "Sample")
abline(0,1)


## example 3.9 : logarithmic dist using transformations

n <- 1000
theta <- 0.5

u <- runif(n)
v <- runif(n)
x<- floor(  1 +  log(v)/ log(1-(1-theta)^u)  )
hist(x, breaks = 40)
k <- 1:max(x)
p <- -1 / log(1-theta) * theta^k / k
se <- sqrt(p*(1-p) / n)
p.hat <- tabulate(x)/n

print(round(rbind(p.hat, p, se), 3))


rlogarithmic <- function(n, theta){
  stopifnot(all(theta > 0 & theta < 1))
  th <- rep(theta, length = n)
  u <- runif(n)
  v <- runif(n)
  x<- floor(  1 +  log(v)/ log(1-(1-theta)^u)  )
  return(x)
}


## example 3.10

n <- 1000
v <- 2
Z <- matrix(rnorm(n*v, 0, 1)^2, ncol = v, nrow = n)

x <- numeric(n)
for (i in 1:n){
  x[i] <- sum(Z[i,])
}

hist(x, breaks = 20, freq = FALSE)
curve(dchisq(x, df = 2), add = TRUE, col = "orange", lwd = 3)

## Mixture 3.10.5

n <- 1000
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 6, 1)
s <- x1 + x2 

hist(s, breaks = 30)

mix <- numeric(n)

for(i in 1:n){
  k <- sample(c(1,2), size = 1, replace = TRUE)
  
  if (k == 1){
    mix[i] <- x1[i]
  }
  else if (k == 2){
    mix[i] <- x2[i]
  }
}
hist(mix, breaks = 50)

## example 3.11

n <- 1000
x1 <- rgamma(n, 2, 2)
x2 <- rgamma(n, 2, 4)
s <- x1 + x2
m <- numeric(n)
for(i in 1:n){
  k <- sample(c(1,2), size = 1, replace = TRUE)
  
  if (k == 1){
    m[i] <- x1[i]
  }
  else if (k == 2){
    m[i] <- x2[i]
  }
}



par(mfcol=c(1,2))
hist(s, breaks = 20, freq = FALSE, xlim = c(0,5), ylim = c(0,1))
lines(density(m))
hist(m, breaks = 20, freq = FALSE, xlim = c(0,5), ylim = c(0,1))
lines(density(s))

## example 3.12 : mixture of several gamma functions

n <- 1000
r <- 3

X <- matrix(numeric(n*5), ncol = 5)

for (i in 1:n){
  for (j in 1:5){
    X[i,j] <- rgamma(1, r, 1/j)
  }
}

par(mfcol=c(1,1))
M <- (1/15)*X[,1] + (2/15)*X[,2] + (3/15)*X[,3] + (4/15)*X[,4] + (5/15)*X[,5]
hist(M, breaks = 50)
plot(density(M), xlim = c(0,40), ylim = c(0,.3), lwd = 3)
for (i in 1:5){
  lines(density(rgamma(n,3, 1/i)), col = i)
}

## example 3.13

n <- 5000
p <- c(.1, .1, .1, .1, .6)
lam <- c(1, 1.5, 2, 2.5, 3)
k <- sample(1:5, size = n, replace = TRUE, prob = p)
rate <- lam[k]
x <- rgamma(n, shape = 3, rate = rate)

plot(density(x), lwd = 3, ylim = c(0,1))
for (i in 1:5){
  lines(density(rgamma(n,3, i/2 + 1/2)), col = i)
}

## example 3.14: arbitrary mixtures

f <- function(x, lam, theta){
  y <- sum(dgamma(x, 3, lam) * theta)
}

x <- seq(0, 8, length = 200)
dim(x) <- length(x)

y <- apply(x, 1, f, lam = lam, theta = p)

plot(x, y, type = "l", ylim = c(0, .85), lwd =3, ylab = "density")

for (j in 1:5){
  y <- apply(x, 1, dgamma, shape = 3, rate = lam[j])
  lines(x,y)
}

## example 3.15: Poisson-Gamma Mixture

n <- 1000
r <- 4 
beta <- 3
lam <- rgamma(n, r, beta)

x <- rpois(n, lam)

mix <- tabulate(x+1) / n
negbin <- round(dnbinom(0:max(x), r, beta / (1+beta)), 3)
diff <- abs(mix - negbin)
se <- sqrt(negbin * (1- negbin)/n)

round(rbind(mix, negbin, diff, se),3)

## method for generating multivariate normal samples N_{d}(\mu, \Sigma)

n <- 500
d <- 2
mu_val <- rnorm(d, 3, 2)
mu <- matrix(mu_val, ncol = 1)
J <- matrix(rep(1, n), ncol = 1)

Sig <- matrix(numeric(d^2), d, d)
for (i in 1:d){
  for (j in 1:d){
    Sig[i,j] <- 0.99^(abs(i-j))
  }
}

Q <- chol(Sig)
Z <- matrix(rnorm(n * d, 0, 1), nrow = n, ncol = d)

X <- (Z %*% Q) + (J %*% t(mu))

plot(X[,1], X[,2], pch = 19,main = paste("Bivariate Normal Centered at (", round(mu_val[1],2), " , ", round(mu_val[2],2), ")"))
abline(v = mu_val[1], lty = 2, col = "grey")
abline(h = mu_val[2], lty = 2, col = "darkgrey")

## example 3.16

mu <- matrix(0, ncol = 1, nrow = 2)
Sigma <- matrix(c(1, 0.9, 0.9, 1), nrow = 2, ncol = 2)

rmvn.eigen <- function(n, mu, Sigma){
  d <- length(mu)
  ev <- eigen(Sigma, symmetric = TRUE)
  lambda <- ev$values
  V <- ev$vectors
  R <- V %*% diag(sqrt(lambda)) %*% t(V)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  J <- matrix(rep(1, n), ncol = 1)
  X <- (Z %*% R) + (J %*% t(mu))
  X
}

Z <- rmvn.eigen(1000, mu, Sigma)
plot(Z, pch = 19)
abline(v = mu[1], lty = 2, col = "grey")
abline(h = mu[2], lty = 2, col = "darkgrey")

print(colMeans(Z))
print(cor(Z))

## example 3.17

rmvn.svd <- function(n, mu, Sigma){
  d <- length(mu)
  S <- svd(Sigma)
  R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v)
  Z <- matrix(rnorm(n*d), nrow = n, ncol =d)
  X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
  X
}

plot(rmvn.svd(100, mu, Sigma))

## example 3.18 part i

rmvn.Choleski <- function(n, mu, Sigma){
  d <- length(mu)
  Q <- chol(Sigma)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z%*% Q + matrix(mu, n, d, byrow = TRUE)
  X
}

plot(rmvn.Choleski(200, mu, Sigma))

## example 3.18 part ii

data("iris")
y <- subset(x = iris, Species == "virginica")[, 1:4]
mu <- colMeans(y)
Sigma <- cov(y)

# mu
# Sigma

X <- rmvn.Choleski(200, mu, Sigma)

## kinda bootstrapping here

par(mfcol = c(1,2))
pairs(X, pch = 19)

## example 3.19

library(MASS)
library(mvtnorm)

n <- 100
d <- 30
N <- 2000 
mu <- numeric(d)

set.seed(100)
x1 <- system.time(
  for (i in 1:N){
    rmvn.eigen(n, mu, cov(matrix(rnorm(n*d), n, d)))
  }
)
set.seed(100)
x2 <- system.time(
  for (i in 1:N){
    rmvn.svd(n, mu, cov(matrix(rnorm(n*d), n, d)))
  }
)
set.seed(100)
x3 <- system.time(
  for (i in 1:N){
    rmvn.Choleski(n, mu, cov(matrix(rnorm(n*d), n, d)))
  }
)
set.seed(100)
x4 <- system.time(
  for (i in 1:N){
    mvrnorm(n, mu, cov(matrix(rnorm(n*d), n, d)))
  }
)
set.seed(100)
x5 <- system.time(
  for (i in 1:N){
    rmvnorm(n, mu, cov(matrix(rnorm(n*d), n, d)))
  }
)
set.seed(100)
x6 <- system.time(
  for (i in 1:N){
    cov(matrix(rnorm(n*d, n, d)))
  }
)


A <- rbind(x1, x2, x3, x4, x5, x6)
dimnames(A)[[1]] <- c("rmvn.eigen", "rmvn.svd", "rmvn.Choleski",
                      "mvrnorm", "rmvnorm", "generate Sigma")
A <- A[,-c(4,5)]
A <- cbind(A, 2*A[,1])
dimnames(A)[[2]] <- c("user", "system", "elapsed", "textbook")
A

## example 3.20

library(MASS) # for mvrnorm

## a mixture of two multivariate normals with the same covariance
loc.mix.0 <- function(n, p, mu1, mu2, Sigma){
  # d = 2 for BVN
  X <- matrix(0, n, 2) 
  
  for (i in 1:n){
    k <- rbinom(1, size = 1, prob = p)
    if (k == 1){
      X[i, ] <- mvrnorm(1, mu1, Sigma)
    }
    else{
      X[i,] <- mvrnorm(1, mu2, Sigma)
    }
  }
  return(X)
}

Sigma1 <- matrix(c(1,0, 0, 1), ncol = 2)
Sigma2 <- matrix(c(1,0, 0, 5), ncol = 2)

mu1 <- c(10,10)
mu2 <- c(0,0) 

par(mfcol = c(2,1))

A <- loc.mix.0(1000, 0.5, mu1, mu2, Sigma1)
B <- loc.mix.0(1000, 0.5, mu1, mu2, Sigma2)

plot(A)
abline(v = c(-1,1,9,11), h = c(-1,1,9,11), lty = 2)
plot(B)
abline(v = c(-1,1,9,11), h = c(-1,1,9,11), lty = 2)

par(mfcol = c(1,1))

## more efficient version (note no loop)

loc.mix <- function(n, p, mu1, mu2, Sigma){
  
  n1 <- rbinom(1, size = n, prob = p)
  n2 <- n
  x1 <- mvrnorm(n1, mu1, Sigma)
  x2 <- mvrnorm(n2, mu2, Sigma)
  
  X <- rbind(x1, x2)
  
  return(X)
}

A <- loc.mix(1000, 0.5, mu1, mu2, Sigma1)
plot(A)

x <- loc.mix(1000, .5, rep(0,4), 2:5, Sigma = diag(4))
r <- range(x) * 1.2

par(mfcol = c(2,2))
for (i in 1:4){
  hist(x[, i], xlim = r, ylim = c(0, .3), freq = FALSE,
       main = "", breaks = seq(-5,10, .5))
}
par(mfcol = c(1,1))

## Wishart Distribution: can be generated (inefficiently) using MVN

n <- 10000
wseq <- array(numeric(n*4), c(2,2,n))
for (i in 1:n){
  X <- mvrnorm(20, mu = c(0,0), Sigma = diag(2)) # n = 20; d = 2
  W1 <- t(X) %*% X
  W2 <- rWishart(1, df = 20, Sigma = diag(2))[,,1]
  wseq[,,i] <- W1 - W2
}

wseq
B <- apply(wseq, c(1,2), sum)
B/n

## instead using Bartlett's decomposition



Sig <- matrix(numeric(d^2), d, d)
for (i in 1:d){
  for (j in 1:d){
    Sig[i,j] <- 0.5^(abs(i-j))
  }
}

N <- 10000 # num of iterations
n <- 10 ## degrees of freedom of W ~ W_{d}(Sig, n)
d <- 10 ## dimension of Z ~ N(mu, Sig)

wseq <- array(numeric(d*d*N), c(d,d, N))

for (k in 1:N){
  t <- matrix(numeric(d^2), ncol = d)
  
  for (i in 1:d){
    for (j in 1:d){
      if (i > j) {
        t[i,j] <- rnorm(1, 0, 1)
      }
      else if (i == j){
        t[i,j] <- sqrt(rchisq(1, df = n-i+1))
      }
    }
  }
  
  A <- t %*% t(t)
  L <- chol(Sig)
  W1 <- (L %*% A) %*% t(L)
  wseq[,,k] <- W1
}

# verification ?? dubious results
Ws <- rWishart(N, n, Sig)
Diff <- wseq - Ws
apply(Diff, c(1,2), sum)/N

## uniform variates on the d-sphere

n <- 200
d <- 2

U <- array(numeric(1*d*n), c(1,d,n))
for (i in 1:n){
  X <- rnorm(d, 0, 1)
  U[,,i] <- X / norm(X, "2")
}

## set the plot up square
par(pty = "s")
plot(U[1,1,], U[1,2,], pch = 19)
par(pty = "m") #back to normal

## example 3.21 : runif.sphere(n, d)

runif.sphere <- function(n, d){
  U <- array(numeric(1*d*n), c(1,d,n))
  for (i in 1:n){
    X <- rnorm(d, 0, 1)
    U[,,i] <- X / norm(X, "2")
  }
  return(U[1,,])
}

A <- runif.sphere(3000, 3)

#library(scatterplot3d)
# scatterplot3d(A[1,], A[2,], A[3,]) a little basic
# library(plotly)
plot_ly(
  x = A[1,],
  y = A[2,],
  z = A[3,],
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = A[3,],      # optional: color by z-value
    colorscale = "Viridis",
    opacity = 0.8
  )
) %>%
  layout(
    title = "Points on the Unit Sphere",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Z")
    )
  )
































