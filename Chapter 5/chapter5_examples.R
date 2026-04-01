

## example 5.1: visualization of Multivariate Data

data(iris)
pairs(iris[101:150, 1:4]) ## obs 101 to 150 are Virginica 

panel.d <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, .5))
  lines(density(x))
}

x <- scale(iris[101:150, 1:4])
r <- range(x)
pairs(x, diag.panel = panel.d, xlim = r, ylim = r)


hist(iris[101:150, 4], freq = FALSE)
lines(density(iris[101:150, 4]))

library(lattice)
splom(iris[101:150, 1:4])

splom(iris[,1:4], groups = iris$Species) ## groups default to different colors


splom(~iris[1:4], groups = Species, data = iris,
      col = 1, pch = c(1,2,3), cex = c(.5, .5, .5)) ## black and white version with different shapes


## example 5.2: correlation matrix

n <- 1000
X <- runif(n, 0, 1)
Y <- runif(n, 0, 1)
Z <- 5*X - 10*Y

B <- as.matrix(cbind(X, Y, Z), ncol = 3)
cor(B)

install.packages("corrplot")
library(FactoMineR)
library(corrplot)

data("decathlon")
str(decathlon)



corrMat <- cor(decathlon[,1:10])
corrplot(corrMat, type = "upper", tl.col = "black", tl.srt = 45) ## nice visual
corrplot(corrMat, type = "upper", method = "square", addCoef.col = "black",
         diag = FALSE, tl.col = "black")

## example 5.3: Surface Plots and 3D Scatter Plots

f <- function(x, y){
  z <- (1/2 * pi)* exp(-(x^2 + y^2) / 2)
  return(z)
}

# using persp

y <- x <- seq(-3,3, length = 50)
z <- outer(x, y, f)

persp(x, y, z, theta = 45, phi = 30, expand = 0.6, ltheta = 120, shade = 0.5,
      ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "f(x, y)", 
      col = "lightblue", box = FALSE, main = "Standard Bivariate Distribution")


## example 5.4: Add elements to perspective plot

persp(x, y, z, theta = 45, phi = 30,
      expand = .4) -> M
# M

## adding some points of a circle onto the plot
# a <- seq(-pi, pi, pi/16)
# newpts <- cbind(cos(a), sin(a)) * 2
# newpts <- cbind(newpts, 0, 1) # z = 0, t = 1
# N <- newpts %*% M
# points(N[,1]/ N[,4], N[,2]/N[,4], col = 2 )


## adding a nice line on the curved space
x2 <- seq(-3, 3, .1)
y2 <- -x2^2/3
z2 <- f(x2, y2)
N <- cbind(x2, y2, z2, 1) %*% M 
lines(N[,1]/N[,4], N[,2]/N[,4], col = 4, lwd = 3)

y3 <- seq(-3,log(3.1), .1)
x3 <- exp(y3)
z3 <- f(x3, y3)
N <- cbind(x3, y3, z3, 1) %*% M
lines(N[,1]/N[,4], N[,2]/N[,4], col = "red", lwd = 3)

x4 <- c(0, 3.1)
y4 <- c(0, -3.1)
z4 <- f(x4, y4) * 1.2
N <- cbind(x4, y4, z4, 1) %*% M
text(N[1,1]/ N[1,4], N[1,2]/N[1,4], "f(x,y)")

## Example 5.5: wireframe (lattice)
library(lattice)

x <- y <- seq(-3,3, length = 50)

## plotting z~x*y
xy <- expand.grid(x, y)

z <- f(xy[,1], xy[,2])
wireframe(z~ xy[,1]*xy[,2])

## 

install.packages("rgl")
library(rgl)

## example 5.6

library(lattice)
attach(iris)

print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
      data = iris, groups = Species, main = "1", pch = 1:3,
      scales = list(draw = FALSE), zlab = "SL",
      screen = list(z=30, x = -75, y = 0)),
      split = c(1,1,2,2), more = TRUE)

print(cloud(Sepal.Width ~ Petal.Length * Petal.Width, 
            data = iris, groups = Species, main = "2", pch = 1:3,
            scales = list(draw = FALSE), zlab = "SW",
            screen = list(z=30, x = -75, y = 0)),
      split = c(2,1,2,2), more = TRUE)

print(cloud(Petal.Length ~ Sepal.Length * Sepal.Width, 
            data = iris, groups = Species, main = "3", pch = 1:3,
            scales = list(draw = FALSE), zlab = "PL",
            screen = list(z=30, x = -75, y = 0)),
      split = c(1,2,2,2), more = TRUE)

print(cloud(Petal.Width ~ Sepal.Length * Sepal.Width, 
            data = iris, groups = Species, main = "4", pch = 1:3,
            scales = list(draw = FALSE), zlab = "PW",
            screen = list(z=30, x = -75, y = 0)),
      split = c(2,2,2,2), more = TRUE)
detach(iris)

## example 5.7: countour plots

head(volcano)
dim(volcano)
contour(volcano, asp = 1, labcex = 1)
contourplot(volcano)


# example(persp)

## example 5.8: filled contour plots

image(volcano, col = terrain.colors(100), axes = TRUE)
contour(volcano, levels = seq(100, 200, by = 10), add = TRUE)




















