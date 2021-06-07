## Exercise 1: King of Hearts
deck <- read.csv("https://nkha149.github.io/stat385-sp2020/files/data/cards.csv")
deck

## Modify deck values
deck[deck$face == "king", c("value")] <- 14
deck[deck$suit == "hearts" & deck$face != "king", c("value")] <- 10

## Check dataset values
deck[deck$suit == "hearts", ]
deck[deck$face == "king", ]

## Add Jackpot column
deck$jackpot <- (deck$face == "king" & deck$suit == "hearts")
deck

## Write a function that draws 4 cards
draw <- function() {
  cards <- deck[sample(1:nrow(deck), size = 4), ]
  total_sum <- sum(cards$value)
  return(total_sum)
}

set.seed(385)
draw()
set.seed(420)
draw()
set.seed(400)
draw()
set.seed(2020)
draw()

## Write win_jackpot game
win_jackpot <- function() {
  cards <- deck[sample(1:nrow(deck), size = 4), ]
  print(cards)
  any(cards$jackpot) | (length(unique(cards$face)) == 1) | (length(unique(cards$suit)) == 1)
}

set.seed(385)
win_jackpot()

set.seed(35)
win_jackpot()

set.seed(174)
win_jackpot()

set.seed(2260)
win_jackpot()