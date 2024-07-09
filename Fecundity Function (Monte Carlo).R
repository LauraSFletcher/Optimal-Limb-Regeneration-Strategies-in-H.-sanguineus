######## FECUNDITY FUNCTION ##########

#Calculates and returns the fecundity values for all 10 strategies at each combination of state variables (t, mass, c, i, p).

fecundity.mc <- function(t,mass,C_g,i,p) {
  
  fecundity <- list() #Will add fecundity values for each strategy to this vector.
  
  for (strategy in strategies) { #For each possible energy allocation strategy.
    
    death <- 0 #Variable to keep track of whether or not the individual is dead. 
    
    #Timestep conversions. These calculations take into account the fact that the main model uses backward iteration, but within each month we employ forward iteration. 
    date <- t*30 #Convert from months to days.
    
    #Next, convert from days to Julian date. 
    if (date <= 360) { #If 360 or less days have passed (Year 1; t = 1-12)
      juldate <- date
    }
    else if (date>360 & date <= 720) { #If more than 360, but less than 720 days have passed (Year 2; t = 13-24)
      juldate <- date - 360
    }
    else { #More than 720 days have passed (Year 3; t = 25-36)
      juldate <- date - 720
    }
    
    #Adjust Julian date based on start date of model (beginning of first reproductive season)
    juldate <- juldate + start_date
    
    #Ensure that the Julian date stays within the year's bounds (30 day months, 12 month year, results in 360 day year).
    if (juldate > 360) { #If Julian date is larger than maximum 360 days, cap at 360. 
      juldate <- juldate - 360
    }
 
    bodymass <- 0.25*mass #Convert from computer mass values (m; 1-12) to real body masses in g. 
    P_joules <- 32.1672-(1.0387*bodymass) #kJ/g of hepato/ovary tissue (taken from calorimetry data, n = 245; to be published elsewhere)
    P_max <- (0.17*bodymass)*P_joules #Maximum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
    P_min <- (0.03*bodymass)*P_joules #Minimum energy of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022) 
    P_energy <- P_min + (((P_max - P_min) / n) * p) #Calculate the starting energy state from computer p value (1-4; Eq. 2.3, Clark and Mangel, 2000). 
    P <- P_energy - P_min #Calculate the usable energy reserves from the hepato/ovary for reproduction, in kJ. Simply the difference between the starting energy value and the minimum energy value. 
    i.c <- i #This is the starting injury state (1-5). 
    regeneration <- 0 #This variable keeps track of the mass of regenerated tissue, which will be updated throughout the month depending on the energy allocation strategy. 
    if (i.c == i_max) { #If uninjured, set the number of missing limbs to 0.
      injury_degree <- 0
    }
    else {
      injury_degree <- missing_limbs
    }
    month_end <- juldate #This variable represents the date at the end of month/time step; for use in while loop below.
    juldate <- juldate - 29 #Reset to beginning of month (i.e., the end of the last time step).
    if (juldate == 0) {juldate <- 1} #Ensure that the date stays within bounds.
    
    #Daily bioenergetics calculations, looped thirty times to represent a month. 
    while (juldate <= month_end) { 
      
      if (juldate%%3==0) { #If today's date is divisible by three, eat and divide excess energy according to 10 strategies (to simulate eating every three days). 
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
        
        ## DECISION CODE ##
        #There is energy leftover after accounting for metabolism, excretion, and feces. Divide this energy between growth, regeneration, and hepato/ovary energy reserves based on energy allocation strategies.
        else { 
          if (strategy == 1) { #All energy to reproduction.
            P_energy <- strategy1(E,P_energy)
          }
          else if (strategy == 2) { #2/3 reproduction, 1/3 growth.
            P_energy <- strategy2(E,P_energy,bodymass)[[1]]
            bodymass <- strategy2(E,P_energy,bodymass)[[2]]
          }
          else if (strategy == 3) { #2/3 reproduction, 1/3 regeneration.
            P_energy <- strategy3(E,P_energy,regeneration)[[1]]
            regeneration <- strategy3(E,P_energy,regeneration)[[2]]
          }
          else if (strategy == 4) { #1/3 reproduction, 2/3 growth.
            P_energy <- strategy4(E,P_energy,bodymass)[[1]]
            bodymass <- strategy4(E,P_energy,bodymass)[[2]]
          }
          else if (strategy == 5) { #1/3 reproduction, 2/3 regeneration.
            P_energy <- strategy5(E,P_energy,regeneration)[[1]]
            regeneration <- strategy5(E,P_energy,regeneration)[[2]]
          }
          else if (strategy == 6) { #1/3 to each.
            P_energy <- strategy6(E,P_energy,bodymass,regeneration)[[1]]
            bodymass <- strategy6(E,P_energy,bodymass,regeneration)[[2]]
            regeneration <- strategy6(E,P_energy,bodymass,regeneration)[[3]]
          }
          else if (strategy == 7) { #All excess energy to growth.
            bodymass <- strategy7(E,bodymass)
          }
          else if (strategy == 8) { #All excess energy to regeneration.
            regeneration <- strategy8(E,regeneration)
          }
          else if (strategy == 9) { #2/3 growth, 1/3 regeneration.
            bodymass <- strategy9(E,bodymass,regeneration)[[1]]
            regeneration <- strategy9(E,bodymass,regeneration)[[2]]
          }
          else { #Strategy 10; 1/3 growth, 2/3 regeneration.
            bodymass <- strategy10(E,bodymass,regeneration)[[1]]
            regeneration <- strategy10(E,bodymass,regeneration)[[2]]
          }
        }
      }
      
      else { #Today's date is not divisible by three, so do not eat. Deduct metabolic costs only. 
        deficit <- metabolism.mc(juldate,bodymass,latitude)
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
      
      juldate <- juldate + 1 #Calculations for the day are complete; continue to next day of the month.
      
    }#End of monthly while loop
    
    #Alter injury state based on strategy.
    limb_mass <- (0.018206*bodymass - 0.002795) * injury_degree  #The first term here (in parentheses) is the average mass of a single walking limb, in grams, based on body mass. 
    #Taken from samples collected in 2020 from Maine down to North Carolina (n = 799; Griffen et al., 2023).
    #This is then multiplied by the number of missing limbs to get the total mass that must be regenerated for regeneration to be complete.
    
    r_max <- limb_mass/4 #This is the maximum amount of mass that can be regenerated in a single month. It takes a mininum of 4 months to fully regenerate, or 2 molts.
    #Pringle (1990) demonstrated that the congener Hemigrapsus edwardsii took 2 or more molts to completely regenerate limbs; also demonstrated that autotomy induces molting.
    #The same paper showed that injured, mature males molted ~55 days following autotomy (Table 4.3); we will round this up to 60 days (2 months) for a single molt, and therefore four months to completely regenerate (at a minimum). 
    
    if (regeneration >= r_max && i.c < i_max) { #If the crab regenerated the maximum amount of limb mass for the month, move up one injury state.
      i.c <- i.c + 1
    }
    
    #Pull from energy reserves to finance reproduction, if during the breeding season. 
    ovigerous <- breeding(juldate,latitude)
    if (ovigerous == 1) { #If during the breeding season, reproduce. 
      max_eggs <- (3 * bodymass) + 0.5 #The maximum amount of energy that can be put into a clutch of eggs, in kJ, as a function of body mass. Based on calorimetry methods which will be published elsewhere (n = 213 clutches). 
      if (P > max_eggs) { #If the amount of energy reserves available for reproduction is greater than the maximum energy that can be put into a clutch for a given body size, use the maximum and leave the rest for future reproduction. 
        clutch_energy <- max_eggs 
        P_energy <- P_energy-clutch_energy #Pull from energy reserves to finance reproduction. 
      }
      else { #If the amount of available energy reserves is less than or equal to the maximum energy that can be put into a clutch; use all of it. 
        clutch_energy <- P
        P_energy <- P_min #Pull from energy reserves to finance reproduction. 
      }
    }
    else { #It is outside the breeding season, so no reproduction occurs. 
      clutch_energy <- 0
    }
    
    ## LINEAR INTERPOLATION ##
    p.c <- bounds.p(n / (P_max - P_min) * (P_energy - P_min)) #Calculate new computer value for energy; based on Equation 2.3 from Clark and Mangel (2000).
    m.c <- bounds.m(m.c <- m / (massmax - massmin) * (bodymass - massmin)) #Calculate new computer value for body mass; based on Equation 2.3 from Clark and Mangel (2000). 
    
    #Interpolation function.
    np.c <- interpolation.mc(t,m.c,i.c,p.c) #Calculate future fitness based on updated state variables.
    
    #Calculate mortality based on body mass.
    mortality <- mortality(bodymass)
    
    #Incorporate the effect of injury on mortality.
    if (i.c < i_max) { #If still injured at the end of the month, apply higher mortality rate.
      true_mortality <- mortality * injured_mortality
    }
    else {
      true_mortality <- mortality
    }
    
    ## FITNESS ##
    #Fitness is determined by the number of eggs that could be produced this month based on existing energy reserves. 
    total_energy <- clutch_energy*1000 #Convert from kJ to Joules. 
    reproduction <- total_energy/egg_energy #Calculate the number of eggs that could be produced this month.
    lifetime_fitness <- reproduction + (np.c * (1-true_mortality)) #Calculate total lifetime fitness by adding this month's reproduction to expected future fitness.
    
    #If crab is dead, then fitness is 0 (no current or future expected fitness). 
    if (death == 1) { 
      lifetime_fitness <- 0
    }
    
    fecundity <- append(fecundity,lifetime_fitness) #Add the fitness value for this strategy to the list of fecundity values.
    
  } #End of strategy for loop.
  
  return(fecundity) #Returns a list of fecundity values for each combination of state variables. 
  
}