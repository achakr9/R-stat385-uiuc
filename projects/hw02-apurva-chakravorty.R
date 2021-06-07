# First Name: Apurva
# Last Name: Chakravorty
# NetID: achakr9
# Homework 2

###

# Exercise 1: French Roulette
## (a)
# Read the data
wheel <- read.csv("~/Desktop/roulette.csv")
wheel

## (b)
# Function roullete()
low <- 1:18
high <- 19:36
red <- c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
black <- c(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
even <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36)
odd <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35)
first <- 1:12
second <- 13:24
third <- 25:36

roulette <- function(bet, amount = 1) {
  winrow <- wheel[sample(1:nrow(wheel), size = 1), ]
  win <- winrow$number
  if (bet == win) {
    cat('$', 36*amount, sep = '')
  } else if (bet == 'low' & win %in% low) {
    cat('$', amount, sep = '')
  } else if (bet == 'high' & win %in% high) {
    cat('$', amount, sep = '')
  } else if (bet == 'red' & win %in% red) {
    cat('$', amount, sep = '')
  } else if (bet == 'black' & win %in% black) {
    cat('$', amount, sep = '')
  } else if (bet == 'even' & win %in% even) {
    cat('$', amount, sep = '')
  } else if (bet == 'odd' & win %in% odd) {
    cat('$', amount, sep = '')
  } else if (bet == 'first' & win %in% first) {
    cat('$', 2*amount, sep = '')
  } else if (bet == 'second' & win %in% second) {
    cat('$', 2*amount, sep = '')
  } else if (bet == 'third' & win %in% third) {
    cat('$', 2*amount, sep = '')
  } else {
    cat('-$', amount, sep = '')
  }
}

set.seed(385)
roulette(bet = "red", amount = 10)
set.seed(385)
roulette(bet = "first", amount = 15)
set.seed(385)
roulette(bet = "odd", amount = 20)
set.seed(385)
roulette(bet = 9, amount = 10)
set.seed(385)
roulette(bet = 5)

# Exercise 2: Simulations
## (a)
roulette2 <- function(bet, amount = 1) {
  winrow <- wheel[sample(1:nrow(wheel), size = 1), ]
  win <- winrow$number
  if (bet == win) {
    return(36*amount)
  } else if (bet == 'low' & win %in% low) {
    return(amount)
  } else if (bet == 'high' & win %in% high) {
    return(amount)
  } else if (bet == 'red' & win %in% red) {
    return(amount)
  } else if (bet == 'black' & win %in% black) {
    return(amount)
  } else if (bet == 'even' & win %in% even) {
    return(amount)
  } else if (bet == 'odd' & win %in% odd) {
    return(amount)
  } else if (bet == 'first' & win %in% first) {
    return(2*amount)
  } else if (bet == 'second' & win %in% second) {
    return(2*amount)
  } else if (bet == 'third' & win %in% third) {
    return(2*amount)
  } else {
    return(-1*amount)
  }
}

set.seed(385)
roulette2(bet = "red", amount = 10)
set.seed(385)
roulette2(bet = "first", amount = 15)
set.seed(385)
roulette2(bet = "odd", amount = 20)
set.seed(385)
roulette2(bet = 9, amount = 10)
set.seed(385)
roulette2(bet = 5)

## (b)
set.seed(385)
results <- replicate(roulette2(bet = "red"), n = 5000)
mean(results > 0)

## (c)
set.seed(385)
results <- replicate(roulette2(bet = "first"), n = 5000)
mean(results > 0)

## (d)
set.seed(385)
results <- replicate(roulette2(bet = "odd", amount = 5), n = 7500)
mean(results)

# Exercise 3: Iris Flower Dataset
## (a)
iris
hist(x = iris$Sepal.Length, 
     main = 'Frequency of Flowers per Sepal Length', 
     xlab = 'Sepal Lengths', ylab = 'Flower',
     col = "violetred")
box()
grid()

## (b)
plot(x = iris$Sepal.Length, y = iris$Sepal.Width,
     main = 'Respective Lengths and Widths of Iris Flowers',
     xlab = 'Sepal Lengths', ylab = 'Sepal Widths',
     col = 'sienna3', pch = 16)

## (c)
plot(x = iris$Sepal.Length, y = iris$Sepal.Width,
     main = 'Respective Lengths and Widths of Iris Flowers',
     xlab = 'Sepal Lengths', ylab = 'Sepal Widths',
     col = c('seagreen3', 'red3', 'dodgerblue')[unclass(iris$Species)], 
     pch = c(16, 17, 18)[unclass(iris$Species)])
legend(x = "topright", legend = levels(iris$Species), col = c('green3', 'red3', 'dodgerblue'), pch = c(16, 17, 18))
grid()
