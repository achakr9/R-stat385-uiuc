## Exercise 1
roll_dice <- function(){
  die <- 1:6
  dice <- sample(die, size = 6, replace = TRUE)
  avg_sum <- sum(dice)/6
  avg_sum
}

roll_dice()

average_dice <- replicate(n = 100000, expr = roll_dice())
hist(x = average_dice, breaks = seq(from = 0, to = 6, by = 0.5))

## Exercise 2
roll_2dice <- function(red = 1:6, green = 1:6) {
  red_die <- sample(red, size = 1)
  green_die <- sample(green, size = 1)
  if(red_die >= green_die){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

roll_2dice(red = 1:4, green = 5:9)
roll_2dice()

## Exercise 3
instructor <- list("Ha", 24)
students <- list(list("Alex", 20), list("Dave", 21))
stat385 <- list(instructor, students)
stat385

stat385[[2]][[1]][[2]]
stat385[[2]][[2]]
stat385[[1]][[1]]

## Exercise 4
m <- matrix(data = 1:16, nrow = 4, byrow = TRUE)
m

m[1, 3]
m[3, 2]
m[1, ]
m[ , 3]
sum(m[ , 3])
mean(m[1, ])

