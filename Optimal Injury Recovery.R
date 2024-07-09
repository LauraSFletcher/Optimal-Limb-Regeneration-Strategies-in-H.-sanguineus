rm(list = ls()) #Resets R so that nothing transfers over from past iterations.

source("Variables.R") #Sourcing for variables.
source("Functions.R") #Sourcing for functions.
source("Strategy Functions.R") #Sourcing for strategies.
source("Fecundity Function.R") #Sourcing for fecundity function.
source("Graph.R") #Sourcing for graph function.


#####################################
############## ARRAYS ###############
#####################################

F <- array(dim = c(length(T_times), length(mass_range), length(Cstate), length(injury), length(Pstate))) 
#Will be filled with fitness values (i.e., number of egg equivalents).
beststrategy <- array(dim = c(length(T_times), length(mass_range), length(Cstate), length(injury), length(Pstate)))
#Will be filled with optimal strategies 1-10.

F[T,,,,] <- 0 #No reproductive output at end of life (T, or 36 months).
beststrategy[T,,,,] <- 0 #No reproductive output at end of life (T, or 36 months). 


E_proportion <- vector()
body_size <- vector()


###########################################
############### MAIN MODEL ################
###########################################

for (t in rev(T_times)) { #For every month in a 3 year lifespan (1-36); loop through in reverse.
  for (mass in mass_range) { #For every starting mass value (1-12).
    for (c in Cstate) { #For every consumption state (0.5x, 1x, and 2x basal consumption).
      for (i in injury) { #For every injured, 1/3 regenerated, 2/3 regenerated, and uninjured individual (injury status, 1-4).
        for (p in Pstate) { #For every starting energetic state (1-4). 
          
          
          ####################################################
          ############# OPTIMAL INJURY RECOVERY ##############
          ####################################################
          
          V <- fecundity(t,mass,c,i,p) #List of 10 fitness values for current combination of state variables, one from each strategy.
          V <- unlist(V) #Convert list to a vector.
          
          egg_count <- max(V) #Pull the highest egg count from the fitness vector (V).
          optimal <- which.max(V) #Report the position of the highest egg_count in the fitness vector (V); i.e., the optimal strategy. 
          
          if (t < T) { #Fitness array already has values when t = T; this is end of life, so reproductive output = 0; only index for values of t less than T.
            F[t,mass,c,i,p] <- egg_count #Index fitness array with highest egg count.
          }
          
          if (egg_count == 0 && t < T) { #If highest egg count is 0, index beststrategy array with a 0 (i.e., crab is dead, no optimal strategy).  
            beststrategy[t,mass,c,i,p] <- 0
          }
          
          else if (var(V)==0 && t < T) { #If all strategies are equal, no optimal strategy. 
            beststrategy[t,mass,c,i,p] <- 11
          } 
        
          else { #Highest egg count is greater than 0 and not equal to other strategies, so go ahead and index optimal strategy.
            if (t < T) { #Only index if not at end of life.
              beststrategy[t,mass,c,i,p] <- optimal
            }
          }
          
        }#Energy (p) loop
      }#Injury loop
    }#Consumption loop
  }#Mass loop
  print(t) #Print month to keep track of model as it runs.
}#Time (month) loop
graph(c,p,i) #Function to graph results. 

