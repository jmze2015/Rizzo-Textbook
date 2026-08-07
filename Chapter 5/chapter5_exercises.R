
## 5.1
library(MASS)
mu <- c(0,1,2)
sigma <- matrix(c(1,-0.5, 0.5, -0.5, 1, -0.5, 0.5, -0.5,1), ncol = 3)

X <- mvrnorm(200, mu, sigma)
plot(X[,1],X[,2])
abline(v = mean(X[,1]), col = "red")
abline(h = mean(X[,2]), col = "blue")


## 5.2

pairs(X, panel = panel.smooth)

## 5.3

n <- 2000
p1 <- 0.7
p2 <- 1-p1

X <- Y <- numeric(n)

for (i in 1:n){
  u <- runif(1)
  if (u < p1){
    X[i] <- rnorm(1, 0, 1)
  }
  else {
    X[i] <- rnorm(1, 3, 1)
  }
}

for (i in 1:n){
  u <- runif(1)
  if (u < p1){
    Y[i] <- rnorm(1, 0, 1)
  }
  else {
    Y[i] <- rnorm(1, 3, 1)
  }
}


V <- cbind(X,Y)

plot(V)
library(hexbin)
plot(hexbin(V))

## 5.4


























