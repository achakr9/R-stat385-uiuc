# First Name: Apurva
# Last Name: Chakravorty
# NetID: achakr9
# Homework 1

###

# Exercise 1
## (a)
# Read the data
wheel <- read.csv("~/Desktop/roulette.csv")
wheel

## (b)
# Function roullete()
roulette <- function(bet = 14) {
  winrow <- wheel[sample(1:nrow(wheel), size = 1), ]
  win <- winrow$number
  if(bet == win) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

set.seed(385)
roulette(bet = 10)
set.seed(385)
roulette()
set.seed(385)
roulette(bet = 5)
set.seed(420)
roulette(bet = 4)
set.seed(400)
roulette(bet = 36)

###

# Exercise 2
mortgage_num <- function(m, p, r) {
  top = log((r/((m/p)-r))+1)
  bottom = log(1+r)
  n = ceiling(top/bottom)
  return(n)
}

mortgage_num(m = 2000, p = 200000, r = 0.005)
mortgage_num(m = 1800, p = 200000, r = 0.005)
mortgage_num(m = 1500, p = 200000, r = 0.005)

###

# Exercise 3

## (a)
dim(iris)

## (b)
iris[1:6,]

## (c)
mean(iris[iris$Species == 'virginica', 1])

## (d)
sd(iris[iris$Species == 'versicolor', 3])

## (e)
median(iris[iris$Species == 'setosa', 2])

## (f)
dim(iris[iris$Species == 'setosa', ])
## The number of rows of setosa iris flowers in the iris dataset is 50. 

###

# Exercise 4

## (a)
coronavirus <- read.csv("~/Desktop/coronavirus-jan2020.csv")
str(coronavirus)

## (b)
coronavirus[coronavirus$Date.last.updated == "1/26/2020 11:00 AM", 2:5]
