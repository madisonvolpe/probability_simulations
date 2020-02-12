## The Birthday Problem -- Part 2 ## 

## Exploring the probability at least n (n=2, n=3, n=4) people share a birthday 

  # Create function to sample 365 days 
    
    birthdays <- function(x){sample(1:365, x , replace = T)}
    
  # Set the range of x and number of iteration
    n = 200
    iter = 1500 
    
  # Create three matrices for results 
    
    ## two people share a birthday
    results2 = matrix(c(0), nrow = n, ncol = iter)
    
    ## three people share a birthday
    results3 = matrix(c(0), nrow = n, ncol = iter)
    
    ## four people share a birthday 
    results4 = matrix(c(0), nrow = n, ncol = iter)
    
    
    set.seed(717)
  
  # Loop through iteration (how many times you ask)
    
    for(k in 1:iter){
      
      # Loop through number of people you ask 
      
      for(i in 1:n){
        
        # ask this many people their birthday 
        
        ask_birthday_results <- birthdays(i)
        
          # put results into matrices 
            
            # at least two people share a bday
            results2[i,k] <- ifelse(sum(table(ask_birthday_results)==2)>0,1,0)
            
            # at least three people share a bday
            results3[i,k] <- ifelse(sum(table(ask_birthday_results)==3)>0,1,0)
            
            # at least four people share a bday
            results4[i,k] <- ifelse(sum(table(ask_birthday_results)==4)>0,1,0)
            
      }
    }
  
    ## plot the results
    
    proportion_2people <- rowSums(results2)/iter
    proportion_3people <- rowSums(results3)/iter
    proportion_4people <- rowSums(results4)/iter 
    
      # base R
        xaxis <- 1:200
        
        plot(proportion_2people ~ xaxis, type = "l", ylim = c(0,1),
             main = "Birthday Simulation", col = "black", 
             ylab = "Probability at least N share a birthday", 
             xlab = "Number of People")
        
        lines(proportion_3people ~ xaxis, col = "red")
    
        lines(proportion_4people ~ xaxis, col = "green")
        
        legend("topleft", c("N=2", "N=3", "N=4"), col = c("black", "red", "green"))
        
      # ggplot2 
        
        library(tidyverse)
        
        pro_df <- rbind(data.frame(no_people=1:200, pro=proportion_2people, group = "N=2"),
              data.frame(no_people=1:200, pro=proportion_3people, group = "N=3"),
              data.frame(no_people=1:200, pro=proportion_4people, group = "N=4"))
        
        ggplot(pro_df, aes(x=no_people, y = pro, col = factor(group))) +
          geom_line() +
          ggtitle("Birthday Simulation")+ 
          xlab("No of People") +
          ylab("Probability at least N share a birthday") +
          theme(legend.title=element_blank())
        
               