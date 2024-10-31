rm(list = ls()) #Resets R so that nothing transfers over from past iterations.
detach(data)

source("Variables (Monte Carlo).R") #Sourcing for variables.
source("Functions.R") #Sourcing for functions.
source("Strategy Functions.R") #Sourcing for strategies.
source("Fecundity Function (Monte Carlo).R") #Sourcing for fecundity function. 

### COMPARISON DATA ###
data <- read.csv("~/MC Comparison Data.csv", header = T) #import data. Note: user will have to edit file path to the place where this data file is stored. 
attach(data) #attach the file so column names can be called independently of file name
names(data) #list column names in file

### SETTING UP TEST VECTORS ###
Body <- data$Start_BodyMass #Body size for each test individual at beginning of experiment and simulations. 
P_density <- 32.1672-(1.0387*Body) #Energy density of ovary+hepato tissue in kJ (taken from calorimetry data, n = 243; to be published elsewhere)
Egg_density <- 32.9725-(2.388*Body) #Energy density of eggs in kJ (taken from calorimetry data, n = 213, to be published elsewhere)
E_storage <- (((data$OvaryMass) + (data$HepatoMass))*P_density) + ((data$EggsDuringExperiment)*Egg_density) #Total energy for each test individual at the end of the experiment.
P_mins <- (0.03*Body)*P_density #Minimum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
Eggs <- ((E_storage-P_mins)*1000)/egg_energy #Number of egg equivalents which could be produced by each individual at the end of the experiment. 
Body2 <- data$End_BodyMass #Body size for each test individual at the end of the experiment. 
Injury <- data$NumLimbsMissing #Number of missing limbs for each test individual.
D1_H <- data$D1_H
D2_H <- data$D2_H
D3_H <- data$D3_H
D4_H <- data$D4_H
D5_H <- data$D5_H
D6_H <- data$D6_H
D7_H <- data$D7_H
D8_H <- data$D8_H
D9_H <- data$D9_H
D10_H <- data$D10_H
D11_H <- data$D11_H
D12_H <- data$D12_H
D13_H <- data$D13_H
D14_H <- data$D14_H
D15_H <- data$D15_H
D16_H <- data$D16_H
D1_Cmass <- data$D1_Cmass
D2_Cmass <- data$D2_Cmass
D3_Cmass <- data$D3_Cmass
D4_Cmass <- data$D4_Cmass
D5_Cmass <- data$D5_Cmass
D6_Cmass <- data$D6_Cmass
D7_Cmass <- data$D7_Cmass
D8_Cmass <- data$D8_Cmass
D9_Cmass <- data$D9_Cmass
D10_Cmass <- data$D10_Cmass
D11_Cmass <- data$D11_Cmass
D12_Cmass <- data$D12_Cmass
D13_Cmass <- data$D13_Cmass
D14_Cmass <- data$D14_Cmass
D15_Cmass <- data$D15_Cmass
D16_Cmass <- data$D16_Cmass

dates <- c(126,129,133,136,140,143,147,150,154,157,161,164,168,171,175,178) #Vector of feeding dates in the experiment. 

### SETTING UP MODEL VECTORS ###

#These are the vectors which will be updated in the loops below. 
optimal_size <- vector() #Records body size  for the optimality model. 
fixed_size1 <- vector() #Records body size for the first fixed allocation model. 
fixed_size2 <- vector() #Records body size for the second fixed allocation model. 

optimal_clutch <- vector() #Records clutch mass for the optimality model. 
fixed_clutch1 <- vector() #Records clutch mass for the first fixed allocation model. 
fixed_clutch2 <- vector() #Records clutch mass for the second fixed allocation model. 

for (allocation in 1:3) { #Loop over 3 different energy allocation strategies. 
  for (crab in 1:40) { #Loop over 40 individuals. 
    missing_limbs <- Injury[crab]
    death <- 0 #Variable to keep track of whether or not the individual is dead. 
    bodymass <- Body[crab]
    if (bodymass > 3) { #Cap body mass at maximum value. 
      bodymass <- 3
    }
    if (bodymass < 0.25) { #Cap body mass at minimum value. 
      bodymass <- 0.25
    }
    P_joules <- 32.1672-(1.0387*bodymass) #kJ/g of hepato/ovary tissue (taken from calorimetry data, n = 243; to be published elsewhere)
    P_max <- (0.17*bodymass)*P_joules #Maximum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
    P_min <- (0.03*bodymass)*P_joules #Minimum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
    P_energy <- (2.8544*bodymass) + 1.1748 #Ovary and hepato energy for gravid individuals which have just produced a clutch of eggs as a function of body mass (Latitudinal project, n = 83) 
    if (P_energy > P_max) {
      P_energy <- P_max
    }
    if (P_energy < P_min) {
      P_energy <- P_min
    }
    P <- P_energy - P_min #Calculate the usable energy reserves from the hepato/ovary for reproduction, in kJ. Simply the difference between the starting energy value and the minimum energy value. 
    if (missing_limbs == 0) {
      i.c <- i_max #If uninjured, start crab with regeneration stage of 5 (fully regenerated). 
    }
    else {
      i.c <- 1 #All injured crabs start with newly injured missing limbs
    }
    regeneration <- 0 #This variable keeps track of the mass of regenerated tissue, which will be updated throughout the month depending on the energy allocation strategy.
    
    # DAILY WHILE LOOP #
    
    for (juldate in 126:179) {
      
      if (juldate %in% dates) { #If today is a feeding day, calculate consumption and allocate energy according to strategy. 
        
        #Set diet quality and consumption amount for the day. 
        if (juldate == 126) {
          H <- D1_H[crab]
          C_g <- D1_Cmass[crab]
        }
        else if(juldate==129) {
          H <- D2_H[crab]
          C_g <- D2_Cmass[crab]
        }
        else if(juldate==133) {
          H <- D3_H[crab]
          C_g <- D3_Cmass[crab]
        }
        else if(juldate==136) {
          H <- D4_H[crab]
          C_g <- D4_Cmass[crab]
        }
        else if(juldate==140) {
          H <- D5_H[crab]
          C_g <- D5_Cmass[crab]
        }
        else if(juldate==143) {
          H <- D6_H[crab]
          C_g <- D6_Cmass[crab]
        }
        else if(juldate==147) {
          H <- D7_H[crab]
          C_g <- D7_Cmass[crab]
        }
        else if(juldate==150) {
          H <- D8_H[crab]
          C_g <- D8_Cmass[crab]
        }
        else if(juldate==154) {
          H <- D9_H[crab]
          C_g <- D9_Cmass[crab]
        }
        else if(juldate==157) {
          H <- D10_H[crab]
          C_g <- D10_Cmass[crab]
        }
        else if(juldate==161) {
          H <- D11_H[crab]
          C_g <- D11_Cmass[crab]
        }
        else if(juldate==164) {
          H <- D12_H[crab]
          C_g <- D12_Cmass[crab]
        }
        else if(juldate==168) {
          H <- D13_H[crab]
          C_g <- D13_Cmass[crab]
        }
        else if(juldate==171) {
          H <- D14_H[crab]
          C_g <- D14_Cmass[crab]
        }
        else if(juldate==175) {
          H <- D15_H[crab]
          C_g <- D15_Cmass[crab]
        }
        else if(juldate==178) {
          H <- D16_H[crab]
          C_g <- D16_Cmass[crab]
        }
        
        ## DYNAMIC STATE VARIABLE MODEL ##
        
        F <- array(dim = c(length(T_times), length(mass_range), length(injury), length(Pstate))) 
        #Will be filled with fitness values (i.e., number of egg equivalents).
        beststrategy <- array(dim = c(length(T_times), length(mass_range), length(injury), length(Pstate)))
        #Will be filled with optimal strategies 1-10.
        
        F[T,,,] <- 0 #No reproductive output at end of life (T, or 36 months).
        beststrategy[T,,,] <- 0 #No reproductive output at end of life (T, or 36 months). 
        
        for (t in rev(T_times)) { #For every month in a 3 year lifespan (1-36); loop through in reverse.
          for (mass in mass_range) { #For every starting mass value (1-12).
            for (i in injury) { #For every regeneration stage (1-5). 
              for (p in Pstate) { #For every starting energetic state (1-4). 
                
                
                ####################################################
                ############# OPTIMAL INJURY RECOVERY ##############
                ####################################################
                
                V <- fecundity.mc(t,mass,C_g,i,p) #List of 10 fitness values for current combination of state variables, one from each strategy.
                V <- unlist(V) #Convert list to a vector.
                
                egg_count <- max(V) #Pull the highest egg count from the fitness vector (V).
                optimal <- which.max(V) #Report the position of the highest egg_count in the fitness vector (V); i.e., the optimal strategy. 
                
                if (t < T) { #Fitness array already has values when t = T; this is end of life, so reproductive output = 0; only index for values of t less than T.
                  F[t,mass,i,p] <- egg_count #Index fitness array with highest egg count.
                }
                
                if (egg_count == 0 && t < T) { #If highest egg count is 0, index beststrategy array with a 0 (i.e., crab is dead, no optimal strategy).  
                  beststrategy[t,mass,i,p] <- 0
                }
                
                else if (var(V)==0 && t < T) { #If all strategies are equal, no optimal strategy. 
                  beststrategy[t,mass,i,p] <- 11
                } 
                
                else { #Highest egg count is greater than 0 and not equal to other strategies, so go ahead and index optimal strategy.
                  if (t < T) { #Only index if not at end of life.
                    beststrategy[t,mass,i,p] <- optimal
                  }
                }
                
              }#Energy (p) loop
            } #Injury (i) loop
          }#Mass (m) loop
        }#Time (month) loop
        
        t <- t_calculator(juldate,bodymass) #Converts date of collection to month corresponding to DSVM
        
        ## BIOENERGETICS UPDATING EQUATION ##
        #Calculating consumption. 
        C_animal <- H * C_g #Calculate the mass of animal tissue consumed. 
        C_algae <- (1-H) * C_g #Calculate the mass of algal tissue consumed. 
        CE_animal <- C_animal * 19.71 #Calculate the energy of consumed animal material, in kJ (19.71 kJ/g for mussel tissue; Griffen, 2014).
        CE_algae <- C_algae * 8.37 #Calculate the energy of consumed algal material, in kJ (8.37 kJ/g for algae; Griffen, 2014).
        C_energy <- CE_animal + CE_algae #Add energy from consumed animal tissue to energy from consumed algal material.
        
        #Calculating energy lost to feces.
        Feces_energy <- feces(C_g)
        
        #Calculating energy lost to excretion.
        Urine_energy <- 0.024 * (C_energy - Feces_energy) #Energy lost to excretion is only 2.4% of absorbed energy (Guerin and Sickle, 1995). 
        
        #Calculating energy lost to metabolism.
        Metabolism_energy <- metabolism.mc(juldate,bodymass,latitude)
        
        #Calculating energy leftover after meeting basal metabolic demands.
        E <- C_energy - Feces_energy - Urine_energy - Metabolism_energy
        
        #Update energy state and body mass based on energy gained or lost today.
        if (E == 0) { #Used up just as much energy as gained in consumption; no change in energy reserves or body mass.
          bodymass <- bodymass
          P_energy <- P_energy
        }
        
        else if (E < 0) { #Used up more energy than gained in consumption; must pull from hepato/ovary and then body mass, if needed. 
          deficit <- E*(-1) #Convert E to a positive value to facilitate calculations. 
          if (deficit == P) { #The energy deficit is equal to the amount of usable hepatopancreas energy. 
            P_energy <- P_min #P_energy is depleted to P_min. 
          }
          else if (deficit > P) { #The energy deficit is greater than the energy available in the hepato and ovary, must pull from non-reproductive tissue for energy.
            P_energy <- P_min #Deplete all usable hepatopancreas energy.
            P_deficit <- deficit - P #Calculate the amount of energy that must be pulled from muscle tissue.
            mass_loss <- (P_deficit/75.6134)*2 #Convert the energy pulled from muscle tissue to mass lost in grams.  
            #We multiply this value by two because muscle catabolism is an inefficient process, so we're assuming only 50% of the energy from muscle tissue can be used (twice the mass must be catabolized).
            #There are 75.6134 kJ per gram of muscle tissue, determined from calorimetry (n=10; Griffen et al. 2023). 
            if (mass_loss > (bodymass/4)) { # We assume that half of a crab's body mass is carapace, leaving half for muscle and other tissue. 
              #Further, it is not biologically realistic that a crab would metabolize all of its muscle tissue. Therefore, a crab can at most metabolize a quarter of its total body mass (1/2 its potential muscle tissue mass). 
              death <- 1 #Mark the crab as dead if the energy deficit exceeds the maximum amount of muscle tissue that can be metabolized. 
              break
            }
            bodymass <- bodymass - mass_loss #Update body mass by subtracting the mass of tissue converted to energy.
          }
          else { #The energy deficit is less than the energy available in hepato/ovary. We can pull from energy storage without having to pull from muscle tissue.
            P_energy <- P_energy - deficit #Subtract the energy deficit from usable hepatopancreas energy reserves. 
          }
        }
        
        else { #E > 0. Excess energy may be divided according to optimal or fixed energy allocation strategy.  
          
          if (allocation == 1){ #Divide energy according to optimal strategy. 
            p.c <- bounds.p(n / (P_max - P_min) * (P_energy - P_min)) #Calculate new computer value for energy; based on Equation 2.3 from Clark and Mangel (2000).
            m.c <- bounds.m(m / (massmax - massmin) * (bodymass - massmin)) #Calculate new computer value for body mass; based on Equation 2.3 from Clark and Mangel (2000). 
            optimal_strategy <- beststrategy[t, round(m.c), i.c, round(p.c)] #Select optimal energy allocation strategy based on current state variable values. 
            
            if (optimal_strategy == 1) { #All energy to reproduction.
              P_energy <- strategy1(E,P_energy)
            }
            else if (optimal_strategy == 2) { #2/3 reproduction, 1/3 growth.
              P_energy <- strategy2(E,P_energy,bodymass)[[1]]
              bodymass <- strategy2(E,P_energy,bodymass)[[2]]
            }
            else if (optimal_strategy == 3) { #2/3 reproduction, 1/3 regeneration.
              P_energy <- strategy3(E,P_energy,regeneration)[[1]]
              regeneration <- strategy3(E,P_energy,regeneration)[[2]]
            }
            else if (optimal_strategy == 4) { #1/3 reproduction, 2/3 growth.
              P_energy <- strategy4(E,P_energy,bodymass)[[1]]
              bodymass <- strategy4(E,P_energy,bodymass)[[2]]
            }
            else if (optimal_strategy == 5) { #1/3 reproduction, 2/3 regeneration.
              P_energy <- strategy5(E,P_energy,regeneration)[[1]]
              regeneration <- strategy5(E,P_energy,regeneration)[[2]]
            }
            else if (optimal_strategy == 6) { #1/3 to each.
              P_energy <- strategy6(E,P_energy,bodymass,regeneration)[[1]]
              bodymass <- strategy6(E,P_energy,bodymass,regeneration)[[2]]
              regeneration <- strategy6(E,P_energy,bodymass,regeneration)[[3]]
            }
            else if (optimal_strategy == 7) { #All excess energy to growth.
              bodymass <- strategy7(E,bodymass)
            }
            else if (optimal_strategy == 8) { #All excess energy to regeneration.
              regeneration <- strategy8(E,regeneration)
            }
            else if (optimal_strategy == 9) { #2/3 growth, 1/3 regeneration.
              bodymass <- strategy9(E,bodymass,regeneration)[[1]]
              regeneration <- strategy9(E,bodymass,regeneration)[[2]]
            }
            else if (optimal_strategy == 10) { #Strategy 10; 1/3 growth, 2/3 regeneration.
              bodymass <- strategy10(E,bodymass,regeneration)[[1]]
              regeneration <- strategy10(E,bodymass,regeneration)[[2]]
            }
            else if (optimal_strategy == 0) { #Dead. 
              death <- 1
              break
            }
            else if (optimal_strategy == 11) { #No optimal strategy. Divide energy equally. 
              if (missing_limbs>0) {
                P_energy <- strategy6(E,P_energy,bodymass,regeneration)[[1]]
                bodymass <- strategy6(E,P_energy,bodymass,regeneration)[[2]]
                regeneration <- strategy6(E,P_energy,bodymass,regeneration)[[3]]
              }
              else {
                rep_energy <- E/2 
                P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
                G_energy <- E/2 
                #Calculating mass gained from growth energy.
                #This combines the increased metabolic costs for growth and the energy content of the new biomass.
                G_metabolism <- 7150 #J/g of tissue synthesized.
                #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
                G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
                G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
                G_energy <- G_energy * 1000 #Convert growth energy from kJ to J.
                growth <- G_energy / G_total #Calculate total energy allocated to growth. 
                if (bodymass >= massmax) { #If already at the maximum body size, then growth is not possible. 
                  growth <- 0
                }
                bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
              }
            }
          }
          
          else if (allocation == 2) { #k = 0.9, evenly split (based on Talbot et al., 2019, kappa value for Carcinus maenas)
            kappa <- 0.9
            P_energy <- fixedstrat(kappa,E,P_energy,bodymass,regeneration)[[1]]
            bodymass <- fixedstrat(kappa,E,P_energy,bodymass,regeneration)[[2]]
            regeneration <- fixedstrat(kappa,E,P_energy,bodymass,regeneration)[[3]]
          }
          
          else if (allocation == 3) { #k = 2/3, evenly split
            P_energy <- strategy6(E,P_energy,bodymass,regeneration)[[1]]
            bodymass <- strategy6(E,P_energy,bodymass,regeneration)[[2]]
            regeneration <- strategy6(E,P_energy,bodymass,regeneration)[[3]]
          }
        }
        
      }
      
      else { #It is not a feeding day; simply calculate metabolic costs and subtract from energy stores. 
        
        #Calculating energy lost to metabolism.
        deficit <- metabolism(juldate,bodymass,latitude)
        if (deficit == P) { #The energy deficit is equal to the amount of usable hepatopancreas energy. 
          P_energy <- P_min #P_energy is depleted to P_min. 
        }
        else if (deficit > P) { #The energy deficit is greater than the energy available in the hepato and ovary, must pull from non-reproductive tissue for energy.
          P_energy <- P_min #Deplete all usable hepatopancreas energy.
          P_deficit <- deficit - P #Calculate the amount of energy that must be pulled from muscle tissue.
          mass_loss <- (P_deficit/75.6134)*2 #Convert the energy pulled from muscle tissue to mass lost in grams.  
          #We multiply this value by two because muscle catabolism is an inefficient process, so we're assuming only 50% of the energy from muscle tissue can be used (twice the mass must be catabolized).
          #There are 75.6134 kJ per gram of muscle tissue, determined from calorimetry (n=10; Griffen et al. 2023). 
          if (mass_loss > (bodymass/4)) { # We assume that half of a crab's body mass is carapace, leaving half for muscle and other tissue. 
            #Further, it is not biologically realistic that a crab would metabolize all of its muscle tissue. Therefore, a crab can at most metabolize a quarter of its total body mass (1/2 its potential muscle tissue mass). 
            death <- 1 #Mark the crab as dead if the energy deficit exceeds the maximum amount of muscle tissue that can be metabolized. 
            break
          }
          bodymass <- bodymass - mass_loss #Update body mass by subtracting the mass of tissue converted to energy.
        }
        else { #The energy deficit is less than the energy available in hepato/ovary. We can pull from energy storage without having to pull from muscle tissue.
          P_energy <- P_energy - deficit #Subtract the energy deficit from usable hepatopancreas energy reserves. 
        }
        
      }
      
      if (bodymass > massmax) { #Cap body mass at maximum value of 3 grams. 
        bodymass <- massmax
      }
      P_joules <- 32.1672-(1.0387*bodymass) #kJ/g of hepatopancreas tissue (taken from calorimetry data, n = 245; to be published elsewhere)
      P_max <- (0.17*bodymass)*P_joules #Maximum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
      P_min <- (0.03*bodymass)*P_joules #Minimum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
      if (P_energy > P_max) { #Cap hepato/ovary energy at the new maximum value. 
        P_energy <- P_max
      }
      P <- P_energy - P_min #Calculate the usable energy reserves. 
      
      #If body mass drops below the minimum value of 0.25 grams, the crab is dead, and the while loop is terminated.
      if (bodymass < massmin) { 
        death <- 1 #Mark the crab as dead if its body mass drops below minimum mass (0.25 g). 
        break
      }
      
      if (juldate == 156) { #A month has passed; evaluate regeneration progress. 
        #Alter injury state based on strategy.
        limb_mass <- (0.018206*bodymass - 0.002795) * missing_limbs  #The first term here (in parentheses) is the average mass of a single walking limb, in grams, based on body mass. 
        #Taken from samples collected in 2020 from Maine down to North Carolina (n = 799; Griffen et al., 2023).
        #This is then multiplied by the number of missing limbs to get the total mass that must be regenerated for regeneration to be complete.
        
        r_max <- limb_mass/4 #This is the maximum amount of mass that can be regenerated in a single month. It takes a mininum of 4 months to fully regenerate, or 2 molts.
        #Pringle (1990) demonstrated that the congener Hemigrapsus edwardsii took 2 or more molts to completely regenerate limbs; also demonstrated that autotomy induces molting.
        #The same paper showed that injured, mature males molted ~55 days following autotomy (Table 4.3); we will round this up to 60 days (2 months) for a single molt, and therefore four months to completely regenerate (at a minimum). 
        
        if (regeneration >= r_max && i.c < i_max) { #If the crab regenerated the maximum amount of limb mass for the month, move up one injury state.
          i.c <- i.c + 1
        }
        
        if (i.c == i_max) { #If fully regenerated, change to 0 missing limbs. 
          missing_limbs <- 0
        }
      }
      
    }
    
    # END OF MONTH CALCULATIONS #
    
    if (death == 1) { #If the crab is dead, then end simulation for the individual. 
      break
    }
    
    #Pull from energy reserves to finance reproduction
    reproduction <- (P*1000)/egg_energy #Calculate the number of egg equivalents that could be produced this month.
    
    # END OF LIFE CALCULATIONS #
    
    if (allocation == 1) {
      optimal_size <- c(optimal_size,bodymass) #Add the individual's mean body size at reproduction to the vector of average body sizes. 
      optimal_clutch <- c(optimal_clutch,reproduction) #Add the individual's mean clutch size to the vector of average clutch sizes. 
    }
    
    else if (allocation == 2) {
      fixed_size1 <- c(fixed_size1,bodymass) #Add the individual's mean body size at reproduction to the vector of average body sizes. 
      fixed_clutch1 <- c(fixed_clutch1,reproduction) #Add the individual's mean clutch size to the vector of average clutch sizes. 
    }
    
    else if (allocation == 3) {
      fixed_size2 <- c(fixed_size2,bodymass) #Add the individual's mean body size at reproduction to the vector of average body sizes. 
      fixed_clutch2 <- c(fixed_clutch2,reproduction) #Add the individual's mean clutch size to the vector of average clutch sizes. 
    }
    print(crab) #to keep track of model progress
  }
}


#Save results
all_strategies <- data.frame("Observed Size"=Body2,"Observed Clutch"=Eggs,"Optimal Size"=optimal_size,"Optimal Clutch"=optimal_clutch,"Fixed 1 Size"=fixed_size1,"Fixed 1 Clutch"=fixed_clutch1,"Fixed 2 Size"=fixed_size2,"Fixed 2 Clutch"=fixed_clutch2)

write.csv(all_strategies,"Fixed vs Optimal Results.csv")


### PLOTS ###

plot(Eggs~Body2,xlab="Body Mass (g)",ylab="Clutch Size (#eggs)")
abline(lm(Eggs~Body2))
points(optimal_size,optimal_clutch,col="red")
abline(lm(optimal_clutch~optimal_size),col="red")

plot(Eggs~Body2,xlab="Body Mass (g)",ylab="Clutch Size (#eggs)")
abline(lm(Eggs~Body2))
points(fixed_size1,fixed_clutch1,col="blue")
abline(lm(fixed_clutch1~fixed_size1),col="blue")

plot(Eggs~Body2,xlab="Body Mass (g)",ylab="Clutch Size (#eggs)")
abline(lm(Eggs~Body2))
points(fixed_size2,fixed_clutch2,col="green")
abline(lm(fixed_clutch2~fixed_size2),col="green")


### ANALYSIS ###
detach(new_data)
new_data <- read.csv("~/Documents/Hemi DSVM Final/Results/Fixed vs Optimal Results.csv", header = T) #import data
attach(new_data) #attach the file so column names can be called independently of file name
names(new_data) #list column names in file

summary(lm(Observed.Clutch~Observed.Size)) #significant increase
summary(lm(Optimal.Clutch~Optimal.Size)) #significant increase
summary(lm(Fixed.1.Clutch~Fixed.1.Size)) #significant increase
summary(lm(Fixed.2.Clutch~Fixed.2.Size)) #NO significant increase

optimal_sizes <- c(Observed.Size,Optimal.Size)
optimal_clutches <- c(Observed.Clutch,Optimal.Clutch)
fixed1_sizes <- c(Observed.Size,Fixed.1.Size)
fixed1_clutches <- c(Observed.Clutch,Fixed.1.Clutch)
fixed2_sizes <- c(Observed.Size,Fixed.2.Size)
fixed2_clutches <- c(Observed.Clutch,Fixed.2.Clutch)

data_type <- vector()

for (crab in 1:40) {
  data_type <- c(data_type,"observed")
}

for (crab in 1:40) { 
  data_type <- c(data_type,"simulated")
}

#Testing whether regression intercepts and slopes are significantly different from the observed for each strategy type w/ ANCOVA
summary(lm(optimal_clutches~optimal_sizes*data_type))

summary(lm(fixed1_clutches~fixed1_sizes*data_type)) 

summary(lm(fixed2_clutches~fixed2_sizes*data_type))
