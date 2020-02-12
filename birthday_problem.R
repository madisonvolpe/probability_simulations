## The Birthday Problem Simulation ## 

library(tidyverse)
library(ggplot2)

set.seed(1)

# function to sample 365 days 
  
  # There are 365 days in a year, given a group of people (x), each number will represent a birthday. So birthdays(50), will
  # return 50 days (birthdays), some of which will be the same and some of which will not 

    birthdays <- function(x){
                  sample(1:365, x , replace = T)
                }

# loop - We will use a nested for loop, the nested for loop will repeat 1000 times. Basically we are simulating, asking one
# person their birthday, then another person their birthday up to 50 people. This will be done 1000 times 
# going through the loop is evalauted as birthdays(1) then birthdays(2) then birthdays (3) until birthdays(50) this is then
# repeated 1000 times 
    
n <- 50 
iter <- 1000 

# each row in the results matrix represents a group of people from 1-50. Each column represents an iteration up to 1000
# essentially we are asking 1 person 1000 times their birthday, in which the probability of a birthday match will always be 0 
# if there is only 1 person simulation or not. We are also asking 1000 different groups of 23 people their birthday and then
# recording with each new group (iteration) whether there is a birthday match or not. 

results <- matrix(c(0), nrow = n, ncol = iter)

# the first iteration is ran from 1-50 then the second iteration restarts at 1 to 50, the third follows the same pattern 
# up until 1000 
# when a match is found it is recorded in the results matrix 

for(k in 1:iter){
  for(i in 1:n){
    ask_birthday_results <- birthdays(i)
    results[i,k] <- ifelse(sum(table(birthdays(i))>1)>0, 1, 0)
  }
}

# after each row in the results matrix represents asking that number of people in a group, "n", their birthdays
# 1000 times. So summing the number of times the outcome, a birthday match was observed and dividing by the overall observations
# will give us the P(at least one birthday match) for that group of people 

proportions <- (rowSums(results))/(iter)

# plottting was done using base R and tidyyverse (ggplot2)

# plotting simulation 
prop_df <- data.frame(no_people = 1:50, pro = proportions)

  ## ggplot 2 
  prop_df %>%
    ggplot(aes(x=no_people, y = proportions)) +
      geom_line() +
      ggtitle("Birthday Problem Simulation") +
      xlab("Number of People in the Room") +
      ylab("P(at least one birthday match)")

  ## base
  xaxis <- 1:50
  
  plot(proportions~xaxis, type = "l", ylim=c(0,1),
      xlab = "Number of People in the Room",
      ylab = "Probability at least two share a birthday")
