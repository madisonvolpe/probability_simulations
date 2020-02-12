### Craps Simulation ###

# P(winning first throw) = .22 
# P(winning point roll) =  .27 
# P(winning) = .493

set.seed(71795)

win <- 0

for(i in 1:100000){
  
  # start by rolling 2 die ~ first roll 
    die1 <- sample(1:6,1)
    die2 <- sample(1:6,1)
  
  # calculate first roll 
    first_roll <- sum(die1, die2)
  
  # decide whether you win, lose, or need to roll again based on first roll 
    
    if(first_roll %in% c(7,11)){ # if roll 7, 11 on first roll you win 
      win <- win + 1 
    } else if (first_roll %in% c(2,3,12)){ # if you roll 2,3,12, on first roll you lose 
      win <- win + 0
    } else { # if you do not roll any of these you roll again 
      
      # calculating point roll 
      
      point1 <- sample(1:6,1)
      point2 <- sample(1:6,1)
      
      point_roll <- sum(point1,point2)
    
      # while loop for point rolls
      
      while(point_roll != first_roll & point_roll != 7){ # if point roll is not equal to first roll or 7 then keep on rolling
        
        point1 <- sample(1:6,1)
        point2 <- sample(1:6,1)
        
        point_roll <- sum(point1,point2)
        
        # while loop will stop when a 7 is rolled or the point roll equals the first roll 
      }
      
      if(point_roll == first_roll){ # once while loop stops, if point roll equals first roll then win
         win <- win + 1
      } else if(point_roll == 7) { # if point will equals 7 then lose
        win <- win + 0 
      }
    }
}


win/100000 # 100000 is number of iterations, this should be equal to around .49

