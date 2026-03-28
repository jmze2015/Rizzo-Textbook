
## 1.1

X <- rt(100, df = 4)
MASS::truehist(X, col = "lightblue")

## 1.2
curve(dt(x, df = 4), add = TRUE, col = "orange", lwd = 2)

## 1.3
lines(density(X), col = 2, lwd = 3)

## 1.4
f <- function(x, a, b){
  y <- x-a
  y <- y/b
  return(y)
}

x_init <- rnorm(1000, 2, 2)
x_trans <- f(x_init, a = min(x_init), b = max(x_init) - min(x_init))
# summary(x_init)
# summary(x_trans)
hist(x_init)
hist(x_trans)
z <- f(x_init, mean(x_init), b = sd(x_init))
# summary(z)
hist(z)
sd(z)

## 1.6

mm <- f(x_init, median(x_init), IQR(x_init))
hist(mm, freq = FALSE)

lines(density(z), col = "red", lwd = 3)
lines(density(mm), col = "cyan", lwd = 3)

## the median transformation seems to have higher kurtosis

## 1.7

head(mpg)
ggplot(mpg, aes(displ, hwy, color = drv)) +
  geom_point() + facet_wrap(~class)

## 1.8 (Using Knitr ~ might return to this...)








