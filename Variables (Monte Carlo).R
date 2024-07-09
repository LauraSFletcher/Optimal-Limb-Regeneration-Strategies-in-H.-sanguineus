###########################################################################
############ VARIABLES (DIET QUALITY MONTE CARLO SIMULATIONS) #############
###########################################################################

egg_energy <- 0.197 #This is Joules per egg, determined via calorimetry (n = 189 clutches). 
#These samples were collected from Maine, New Hampshire, Connecticut, New Jersey, and North Carolina.
m <- 12 #This value represents the number of possible starting mass values (0.25 g - 3 g at 0.25 increments, makes 12 possibilities; i.e., the number of bins for interpolation).
mass_range <- 1:m #This vector represents the range of starting body masses. For use in the for loop in the main model. 
T <- 36 #The time horizon of the model in months (a total of 36 timesteps). 
T_times <- 1:T #Used in the main model for loop to count time. 
Cstate <- 1:3 #This represents the range of starting consumption states. For use in the for loop in the main model.  
massmax <- 3 #This value is the maximum starting body mass, in grams.
massmin <- 0.25 #This value is the minimum starting body mass, in grams.
i_max <- 5 #This value represents the total number of possible injury states. 
injury <- 1:i_max #This vector represents the range of possible injury states, with 1 representing completely injured and 4 representing fully regenerated. 
t_submerged <- 0.5 #This is the proportion of time spent submerged each day. 
latitude <- "N" #N for north and S for south. Determines the dates of the reproductive season (begins in March for southern latitudes, and April in northern latitudes) in the metabolism functions.
#Northern latitudes are anything south of New Hampshire, and vice versa. 
if (latitude == "N") {start_date <- 91 } #Julian date of the model's beginning; assuming that crabs become sexually mature at the beginning of the breeding season (April for northern latitudes, March for southern latitudes.)
if (latitude == "S") {start_date <- 60} #Same as above, except the breeding season begins in March for southern latitudes. 
n <- 4 #Represents the number of bins for energy (P) interpolation.
Pstate <- 1:n #Range of starting energy states; for use in main model for loop.
mlength <- 0:m #Used in linear interpolation (mass).
nlength <- 0:n #Used in linear interpolation (energy).
dead <- 0 #Used in linear interpolation. 
injured_mortality <- 1.34 #A multiplier. This value is multiplied by the base mortality value to simulate higher mortality for injured individuals. 
#Based on 34% higher mortality for injured individuals compared to uninjured individuals in juvenile Callinectes sapidus; Smith, 1995.
strategies <- 1:10 #Range of possible energy allocation strategies, 1-10. For use in the fecundity function. 


