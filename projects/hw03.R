# First Name: Apurva
# Last Name: Chakravorty
# NetID: achakr9
# Homework 3

###

## Exercise 1: Vectorized Code

## (a)

specific_sum_vec <- function(m, pos) {
  total <- 0
  new <- m[1:nrow(m), pos]
  total <- sum(new)
  total
}

m <- matrix(data = c(114:133), nrow = 4, byrow = TRUE)
pos <- c(1, 4, 2)
specific_sum_vec(m, pos)

## (b)

long_name_vec <- function(names) {
  output <- names[nchar(names) >= 8]
  output
}

my_names <- c("alexander", "david", "sebastian", "johnathan", "christopher", "ha",
              "washington", "lincoln", "maximo", "mason", "luca", "anthony", "kevin")
long_name_vec(names = my_names)

## (c)

roulette_vec <- function(many_bets) {
  prize <- 0
  total_prize <- c()
  win_lose_random <- sample(x = c(TRUE, FALSE), size = length(many_bets),
                            replace = TRUE)
  prize_amount <- c("low" = 10, "high" = 10, "red" = 20, "black" = 20, 
                    "odd" = 15, "even" = 15, "first" = 50, "second" = 50, "third" = 50)
  prize_bets <- unname(prize_amount[many_bets])
  prize_bets[win_lose_random == FALSE] <- 0
  prize_bets
}

set.seed(385)
roulette_vec(many_bets = c("red"))
roulette_vec(many_bets = c("red", "black", "low", "high"))

long_vec <- rep(c("red", "black", "low", "high", "second", "first", "third",
                  "odd", "even"), 10000)
system.time(roulette_vec(many_bets = long_vec))

###---------------------------------------------------------------------------###

## Exercise 2

## (a) 
par(mfrow = c(1, 1))
hist(x = iris$Sepal.Length, main = "Histogram of Sepal Length", xlab = "Sepal Length",
     breaks = 15, border = "dodgerblue", probability = TRUE,
     ylim = c(0, 0.65), xlim = c(4, 8.2))
box()
grid()
irismean <- mean(iris$Sepal.Length)
abline(v = irismean, col = "darkorange", lwd = 3)
text(x = 6.5, y = 0.6, "x = 5.8433")

## (b)
faithful
hist(x = faithful$eruptions, main = "Histogram of Faithful Eruption Time and Density",
     xlab = "Eruption Time", breaks = 18, border = "dodgerblue", probability = TRUE,
     xlim = c(1, 6), ylim = c(0, 0.8))
box()
grid()
lines(density(faithful$eruptions), col = "darkorange")

## (c)
par(mfrow = c(1, 2))
hist(x = faithful$waiting, main = "Histogram of Faithful Eruption Time and Density",
     xlab = "Eruption Waiting Time", breaks = 12, border = "dodgerblue", probability = TRUE)
box()
grid()
plot(x = faithful$waiting, y = faithful$eruptions, main = "Waiting Time vs. Eruption Time",
     xlab = "Waiting Time (mins)", ylab = "Eruption Time (mins)", pch = 16, col = "dodgerblue")
box()
grid()

###---------------------------------------------------------------------------###
