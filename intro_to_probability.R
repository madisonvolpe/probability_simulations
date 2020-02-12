# Introduction to Probability (APSTA-GE 2351) #
library(tidyverse)

## Have R flip a coin 10 times and count the number of heads
## Repeat this 8 times
## Store the number of heads for each one

set.seed(12345)

coin_flip_8 <- list()
sum_flip_8 <- vector()

for(i in 1:8){
  coin_flip_8[[i]] <- sample(0:1, 10, replace = T)
  sum_flip_8[i] <- sum(coin_flip_8[[i]])
}

## Have R flip a coin 10 times, and count the number of heads of the 10 flips 
## Store this number and repeat 1000 times. Print a table of the number of heads 

set.seed(717)

coin_flip_1000 <- list()
nheads_1000 <- vector() 


for(i in 1:1000){
  coin_flip_1000[[i]] <- sample(0:1, 10, replace = T)
  nheads_1000[i] <- sum(coin_flip_1000[[i]])
}

table(nheads_1000)

## USE R to build the sample space for sampling 2 balls with replacement from an urn 
## containing three balls labeled 1-3. 

get_SS <- function(n){
  outcome <- sample(1:3, n, replace = T)
  outcome <- str_flatten(outcome, collapse = ", ")
  return(outcome)
}

set.seed(716)

outcomes <- vector()

for(i in 1:10000){
  outcomes[i] <- get_SS(2)
}

unique(outcomes)

## The same thing but without replacement

get_SS_wo <- function(n){
  outcome <- sample(1:3, n, replace = F)
  outcome <- str_flatten(outcome, collapse = ", ")
  return(outcome)
}

set.seed (716)

outcomes <- vector()

for(i in 1:10000){
  outcomes[i] <- get_SS_wo(2)
}

unique(outcomes)


## Homework Questions 

# 1 Simulate rolling 2 dice using the RollDie() function. Do this 1000 times and store the results. 

 roll_dice <- function(){  
                        sample(1:6, 2, replace = T)
 }
 
 roll_die_1000 <- list()
 
 set.seed(500)
 
 for(i in 1:1000){
   roll_die_1000[[i]] <- sum(roll_dice())
 }
 
 results <- unlist(roll_die_1000)
 
# 2 Probability of rolling a seven 
  (sum(results %in% 7)/length(results))
 
# 3 Probability of rolling an eleven
 (sum(results %in% 11)/length(results))

# 4 Probability of rolling a seven or an eleven   
 (sum(results %in% 7)/length(results)) + (sum(results %in% 11)/length(results))
 
# 5  You play the game 10 times and win $30. You think, boy this game is easy to win at! I should keep
#    playing! Is this the correct assumption? Prove it with a simulation by calculating your mean earning
#    over 10000 iterations, as well as the your net earning (the sum over the 10000 iterations). Hint: Write a
#    for loop where you keep track of the sum of the two dice rolls, and then in the Earning vector, keep
#    track of the monetary transaction that would occur. Then take the mean and sum of the Earning
#    vector. You may want to look up how to use the if command
 
 
 # roll 7 receive $3
 # roll 11 receive $5
 # any other combo lose $.70 
 
 set.seed(1022)
 
 game_outcome <- vector()
 earning <- vector()
 
 for(i in 1:1000){
  
   game_outcome[i] <- sum(sample(1:6, 2 ,replace = T))
    
    if(game_outcome[i]==7){
    
      earning[i] <- 3
    
    } else if (game_outcome[i]==11){
      
      earning[i] <- 5
      
    } else {
      
      earning[i] <- -.70
      
    }
 }
 
mean(earning) # mean earning per game is .26 cents
sum(earning)  # sum of earnings after playing 1000 times is $267
