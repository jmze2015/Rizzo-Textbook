

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


X <- matrix(floor(10*runif(10*20)), ncol = 10)
image(X, col = terrain.colors(100), axes = TRUE)


library(MASS)

x <- y <- seq(-3, 3, length = 50)

Z <- matrix(numeric(50*50), ncol = 50)

for (i in 1:50){
  for (j in 1:50){
    Z[i, j] <- dnorm(x[i], 0, 2)*dnorm(y[j], 0, 2)
  }
}

image(round(Z,2), col = terrain.colors(100), axes = TRUE)
contour(round(Z,2), levels = seq(0,0.25, by = 0.01), add = TRUE)

library(lattice)
filled.contour(volcano, color = terrain.colors, asp = 1)
levelplot(volcano, scales = list(draw = FALSE),
          xlab = "", ylab = "")

## example 5.9: 2D Histogram

install.packages("hexbin")
library(hexbin)

x <- matrix(rnorm(4000), 2000, 2)
plot(hexbin(x[,1], x[,2]))

## can also use ggplot version

library(ggplot2)
x <- data.frame(x)
ggplot(x, aes(x[,1], x[,2])) + geom_hex()

install.packages("gplots")
library(gplots)
hist2d(x, nbins = 30, 
       col = c("white", rev(terrain.colors(30))))

## example 5.10: Andrew Curves

install.packages("DAAG")
library(DAAG)
data("leafshape17")
df <- leafshape17

View(df)

f <- function(a, v){
  v[1]/sqrt(2) + v[2]*sin(a) + v[3]*cos(a)
}

## scaling data to -1, 1

x <- cbind(df$bladelen, df$petiole, df$bladewid)
n <- nrow(x)
mins <- apply(x, 2, min) # column minimums
maxs <- apply(x, 2, max) # column maximums
r <- maxs - mins
y <- sweep(x, 2, mins)
y <- sweep(y, 2, r, "/")
x <- 2 * y -1

## plot set up
plot(0,0, xlim = c(-pi, pi), ylim = c(-3,3),
     xlab = "t", ylab = "Andrew Curves", main = "",
     type = "n")

a <- seq(-pi, pi, len = 101)
dim(a) <- length(a)
for (i in 1:n){
  g <- arch[i] + 1
  y <- apply(a, MARGIN = 1, FUN = f, v = x[i,])
  lines(a, y, lty = g)
}
legend(3, c("Orthotropic", "Plagiotropic"), lty = 1:2)

## example 5.11" Parallel Coordinates

library(lattice)
library(MASS)

x <- crabs[seq(5, 200, 5),] ## retrieves every 5th obs.
parallelplot(~x[4:8] | sp*sex, x)


#trellis.device(color = FALSE)
x <- crabs[seq(5, 200, 5),]
a <- x$CW * x$CL


x[4:8] <- x[4:8] / sqrt(a)


parallelplot(~x[4:8] | sp*sex, x)

## example 5.12: segment plot

x <- MASS::crabs[seq(5, 200, 5),]
x <- subset(x, sex == "M")
a <- x$CW * x$CL
X[4:8] <- x[4:8] / sqrt(a)
palette("default")
stars(x[4:8], draw.segments = TRUE,
      labels = levels(x$sp), nrow = 4,
      ylim = c(-2, 10), key.loc = c(3, -1))
dim(x[4:8])

##




























