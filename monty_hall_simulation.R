### Monty Hall Simulation ###

monty <- function(strat, N){
  
  # N is the number of iterations 
  # strat = 'stay', 'switch', 'random' is the strategy you use 
  
  doors <-  1:3 #initialize the doors behind one of which is a good prize
  win <- 0  #to keep track of number of wins 
  
    for(i in 1:N){
      
      prize <- sample(1:3, 1) #randomize which door has the good prize
      guess <- sample(1:3, 1) #guess a door at random
      
      ## reveal one of the doors you didn't pick which has a bum prize
      if(prize != guess){
        reveal <- doors[doors != prize & doors != guess]
      } else {
        reveal <- sample(doors[doors != prize], 1)
      }
      
      ## stay with your initial guess or switch
      if(strat == 'switch'){
        select <- doors[doors != reveal & doors != guess]
      }
      
      if(strat == 'stay'){
        select <- doors[doors == guess]
      }
      
      if(strat == 'random'){
        select <- sample(doors[doors != reveal],1)
      }
      
      ## count up your wins 
      
      if(select == prize){
        
        win <- win + 1
        
      }
      # print(paste('Using the', strat, 'strategy, your win percentage was',
      #       win/N*100, '%', sep = ' ')) # print win percentage of your strategy 
      
    }
  print((win/N)*100)
}


set.seed(717)
monty(strat = 'stay', 1000) # 1/3 is about 33.3% so this makes sense 
monty(strat = 'switch', 1000) # 2/3 is about 66.6 % so this makes sense
monty(strat = 'random', 1000)
