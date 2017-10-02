## ------------------------------------------------------------------------
library(boxlinre)
library(ggplot2)
library(gridExtra)

data(iris)

## ------------------------------------------------------------------------
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
m = linreg(formula, iris)
summary(m)

## ------------------------------------------------------------------------
summary(resid(m))

