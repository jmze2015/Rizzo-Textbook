
## example 1.1

sumdice <- function(n, sides = 6){
  if (sides < 1){
    stop("not a valid number of sides")
  }
  k <- sample(1:sides, size = n, replace = TRUE)
  return(sum(k))
}

sumdice(100, sides = 6)/100 ## should be close to 3.5 

## example 1.2

data("iris")
names(iris)
table(iris$Species)
head(iris)
w <- iris$Sepal.Width
mean(w)


## example 1.3

x <- 1:24
matrix(1:24, nrow = 4)
x <- array(1:48, c(3,4,2,2)) ## wow cool

## example 1.4

A <- matrix(0, nrow = 2, ncol = 2)
B <- matrix(1, 2, 2)
C <- matrix(1:8, nrow = 2, byrow = TRUE)
D <- matrix(1:8, nrow = 2)

## example 1.5

x <- as.matrix(iris[, 1:4])
mean(x[,2])
mean(x[51:100, 3]) ## versicolor mean petal.length
y <- array(x, dim = c(50, 3, 4))
head(y)
head(iris)
mean(y[,,2])
mean(y[,2,3])

## example 1.6

n <- 1000
x <- rbinom(n, size =1 , prob = 0.5)
table(x)
head(x)
r <- rle(x)
str(r)

## example 1.7

w <- wilcox.test(rnorm(10), rnorm(10,2))
w$statistic
w$p.value

unlist(w)
unclass(w)

## example 1.8

set.seed(123)
a <- matrix(runif(8), 4, 2)
dimnames(a) <- list(c("GG", "WP", "NO", "RE"), c("x", "y"))
a["GG", "x"]
a["WP", "y"]

## example 1.9

boxplot(iris$Sepal.Length ~ iris$Species,
        ylab = "Sepal Length", xlab = "Species",
        boxwex = 0.4, col = c("red", "orange", "lightblue"))

## example 1.10

plot(0:25, rep(1, 26), pch = 0:25, col = 0:25)
text(0:25, 0.9, 0:25, col = 24:49)

## example 1.11

barplot(table(r$lengths))
library(ggplot2)
df <- data.frame(lengths = factor(r$lengths))
ggplot(df, aes(lengths)) + geom_bar()

## example 1.12

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width,
                        color = Species, shape = Species)) +
  geom_point(size = 2)

## example 1.13

ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot() + coord_flip() 
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_violin() + coord_flip()

## example 1.14

?mpg
head(mpg)
class(mpg)
dim(mpg)
unique(mpg$manufacturer)

ggplot(mpg, aes(displ, hwy, color = manufacturer)) +
  geom_point() + facet_wrap(~ class)

## example 1.15

forearm <- scan(file = "./Small-Data-Sets/FOREARM.DAT")
print(forearm)
boxplot(forearm, horizontal = TRUE, xlab = "Forearm Length (cm) ??")

## example 1.16

fileloc <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
df <- read.table(file = fileloc, na.strings = "?", as.is = TRUE)
head(df)
colnames(df) <- c("mpg", "cyl", "displ", "hp", "wt", "accel", 
                  "year", "origin", "name")
summary(df)

length(unique(df$name))
length(df$name)

## example 1.17

dates <- c("3/27/1995", "4/3/1995",
           "4/10/1995", "4/18/1995")
prices <- c(11.1, 7.9, 1.9, 7.3)
d <- data.frame(Dates = dates, Prices = prices)

filename <- "temp.csv"
write.table(d, file = filename, sep = ',',
            row.names = FALSE)


read.table(file = filename, sep = ",", header = TRUE)


