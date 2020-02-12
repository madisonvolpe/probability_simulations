## Two Stage Simulations ## 

## A ball is drawn from an urn containing an equal number of red, white, and blue balls. If the ball drawn is white, a fair 
## coin is flipped and the outcome is recorded. If the ball is blue, a card is drawn from a standard (52 card) deck and the suit
## (club, diamond, heart, and spade, present in equal proportions in the deck) is recorded. If the ball is red, a fair six-sided die
## is rolled and the top number is recorded ## 

  
  ## Functions 

  select_balls <- function(n = 1){
    sample(c("Red", "White", "Blue"), size = 1)
  }
  
  flip_coin <- function(x = 1){
    sample(c("H", "T"), size =1)
  }
  
  pick_card <- function(j = 1){
    sample(c("club", "diamond", "heart", "spade"), size =1)
  }
  
  roll_die <- function(m = 1){
    sample(1:6, size = 1)
  }
  
  # outcome vectors 
  
  ball_drawn <- vector()
  outcome <- vector()
  
  # set seed
  
  set.seed(717)
  
  for (i in 1:100000){
    
    # draw a ball, whatever the ball is will determine the next step 
    ball_drawn[i] <- select_balls() 
    
    if(ball_drawn[i] == "White"){ # if the white ball is drawn flip a fair coin
      
      outcome[i] <- flip_coin()
      
    } else if (ball_drawn[i] == "Blue"){ # if blue ball is drawn pick a card
      
      outcome[i] <- pick_card()
      
    } else {  # if the red ball is drawn roll die 
      
      outcome[i] <- roll_die()
      
      }
  }
  
  library(tidyverse)
  
  df_ss <- data.frame(ball = ball_drawn, outcome = outcome)
  ss <- distinct(df_ss)
  
  ## What is the probability that the die rolls an odd number? 
  nrow(df_ss[df_ss$outcome %in% c("1", "3", "5"),])/100000
  
  ## What is the probability that a blue ball is drawn and a heart is drawn ?
  nrow(df_ss[df_ss$outcome == 'heart',])/100000
  
  ## Are the events ~ Blue ball is Drawn ~ & ~Heart is drawn~ independent?
  
    ## Event A is Heart is Drawn
    ## Event  B is Blue ball is Drawn 
  
    A <- nrow(df_ss[df_ss$outcome == 'heart',])/100000
    B <- nrow(df_ss[df_ss$ball == 'Blue',])/100000
  
    ## P(Heart | Blue) 
    A/B 
    
    ## These events are dependent because the probability of drawing heart and the conditional probability
    ## of drawing a heart given the ball is blue are different. 
    