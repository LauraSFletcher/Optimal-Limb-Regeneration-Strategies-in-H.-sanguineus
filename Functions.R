########################################################
###################### MORTALITY #######################
########################################################

#This function calculates mortality based on computer body mass values. Mortality decreases linearly with body size. 
mortality <- function(bodymass) {
  mass <- 12 / (massmax - massmin) * (bodymass - massmin) #Convert to computer mass values (1-12); based on equation 2.3 from Clark and Mangel (2000). 
  mort <- 0.057 - (0.0038*mass) #Mortality decreases linearly with body mass.  
  #A body mass of 6 or 1.5 g should have 0.038 mortality, basal monthly mortality calculated from Fukui, 1988
  #body mass of 1 (0.25 g) should have 1.5x that mortality (0.057), and a body mass of 12 (3 g) should have ~ 0.5x that mortality.
  #mort <- 0.54167 - (mass*0.04167) #Test mortality
  return(mort)
}


#########################################################
################## LINEAR INTERPOLATION #################
#########################################################

#This function ensures that computer energy (P) values are within bounds to pull from the fitness array. 
bounds.p <- function(E) { 
  x <- min(E, n) #Value must be smaller than n (maximum computer value).
  xx <- max(x, 1) #And greater than 1 (minimum computer value).
  return(xx)
}

#This function ensures that computer body mass (m) values are within bounds to pull from the fitness array.
bounds.m <- function(M) { 
  x <- min(M, m) #Value must be smaller than m (maximum computer value).
  xx <- max(x, 1) #And greater than 1 (minimum mass value, 0.25 g).
  return(xx)
}

#A function that pulls fitness values from the fitness array based on the current combination of state variables. 
fitness <- function(t, mass, c, i, p) { #fitness function; used in linear interpolation
  fitness <- F[t, mass, c, i, p] 
}

#The main linear interpolation function. The interpolation() function takes computer values as an input and uses them to calculate future fitness. 
interpolation <- function(t, m.c, c, i.c, p.c) { 
 
  #Equation 2.7 (Mangel, 2000) for energy. 
  j <- floor(p.c)
  j2 <- ceiling(p.c)
  low.p <- p.c - j  #Used for change in p.c.
  high.p <- j2 - p.c #Used for 1 - change in p.c.
  
  #Equation 2.7 (Mangel, 2000) for body mass.
  k <- floor(m.c)
  k2 <- ceiling(m.c)
  low.m <- m.c - k  #Used for change in p.c.
  high.m <- k2 - m.c #Used for 1 - change in p.c.
  
  #calculate future expected fitness based on current strategy. 
  np.c <-
    if (t ==T) { #If at the start of the simulation and end of the crab's life, pull from fitness array with t instead of t + 1.
      if(any(p.c==nlength[-1]) && any(m.c==mlength[-1])) { #If both p.c and m.c are integers, use them both.
        fitness(t,m.c,c,i.c,p.c)
      }
      else if (any(p.c==nlength[-1]) && any(m.c!=mlength[-1])) { #If p.c is an integer, use it.
        high.m * fitness(t,k,c,i.c,p.c) + low.m * fitness(t,k2,c,i.c,p.c)
      }
      else if (any(p.c!=nlength[-1]) && any(m.c==mlength[-1])) { #If m.c is an integer, use it.
        high.p * fitness(t,m.c,c,i.c,j) + low.p * fitness(t,m.c,c,i.c,j2)
      }
      else { #Neither m.c nor p.c are integers (equation 2.9; Mangel, 2000). 
        high.p * high.m * fitness(t,k,c,i.c,j) + high.p * low.m * fitness(t,k2,c,i.c,j) + low.p * high.m * fitness(t,k,c,i.c,j2) + low.p * low.m * fitness(t,k2,c,i.c,j2)
      }
    } else { #t != T, i.e. crab hasn't reached end of life yet, must use t + 1 to pull from fitness array. 
      if(any(p.c==nlength[-1]) && any(m.c==mlength[-1])) { #If both p.c and m.c are integers, use them both.
        fitness(t+1,m.c,c,i.c,p.c)
      }
      else if (any(p.c==nlength[-1]) && any(m.c!=mlength[-1])) { #i=If p.c is an integer, use it.
        high.m * fitness(t+1,k,c,i.c,p.c) + low.m * fitness(t+1,k2,c,i.c,p.c)
      }
      else if (any(p.c!=nlength[-1]) && any(m.c==mlength[-1])) { #If m.c is an integer, use it.
        high.p * fitness(t+1,m.c,c,i.c,j) + low.p * fitness(t+1,m.c,c,i.c,j2)
      }
      else { #Neither m.c nor p.c are integers (equation 2.9; Mangel, 2000). 
        high.p * high.m * fitness(t+1,k,c,i.c,j) + high.p * low.m * fitness(t+1,k2,c,i.c,j) + low.p * high.m * fitness(t+1,k,c,i.c,j2) + low.p * low.m * fitness(t+1,k2,c,i.c,j2)
      }
    }
  return(np.c)
}

#A function that pulls fitness values from the fitness array based on the current combination of state variables. 
fitness.mc <- function(t,mass,i,p) { #fitness function; used in linear interpolation for monte carlo simulations
  fitness <- F[t,mass,i,p] 
}

#The main linear interpolation function. The interpolation() function takes computer values as an input and uses them to calculate future fitness. 
interpolation.mc <- function(t,m.c,i.c,p.c) { 
  
  #Equation 2.7 (Mangel, 2000) for energy. 
  j <- floor(p.c)
  j2 <- ceiling(p.c)
  low.p <- p.c - j  #Used for change in p.c.
  high.p <- j2 - p.c #Used for 1 - change in p.c.
  
  #Equation 2.7 (Mangel, 2000) for body mass.
  k <- floor(m.c)
  k2 <- ceiling(m.c)
  low.m <- m.c - k  #Used for change in p.c.
  high.m <- k2 - m.c #Used for 1 - change in p.c.
  
  #calculate future expected fitness based on current strategy. 
  np.c <-
    if (t ==T) { #If at the start of the simulation and end of the crab's life, pull from fitness array with t instead of t + 1.
      if(any(p.c==nlength[-1]) && any(m.c==mlength[-1])) { #If both p.c and m.c are integers, use them both.
        fitness.mc(t,m.c,i.c,p.c)
      }
      else if (any(p.c==nlength[-1]) && any(m.c!=mlength[-1])) { #If p.c is an integer, use it.
        high.m * fitness.mc(t,k,i.c,p.c) + low.m * fitness.mc(t,k2,i.c,p.c)
      }
      else if (any(p.c!=nlength[-1]) && any(m.c==mlength[-1])) { #If m.c is an integer, use it.
        high.p * fitness.mc(t,m.c,i.c,j) + low.p * fitness.mc(t,m.c,i.c,j2)
      }
      else { #Neither m.c nor p.c are integers (equation 2.9; Mangel, 2000). 
        high.p * high.m * fitness.mc(t,k,i.c,j) + high.p * low.m * fitness.mc(t,k2,i.c,j) + low.p * high.m * fitness.mc(t,k,i.c,j2) + low.p * low.m * fitness.mc(t,k2,i.c,j2)
      }
    } else { #t != T, i.e. crab hasn't reached end of life yet, must use t + 1 to pull from fitness array. 
      if(any(p.c==nlength[-1]) && any(m.c==mlength[-1])) { #If both p.c and m.c are integers, use them both.
        fitness.mc(t+1,m.c,i.c,p.c)
      }
      else if (any(p.c==nlength[-1]) && any(m.c!=mlength[-1])) { #i=If p.c is an integer, use it.
        high.m * fitness.mc(t+1,k,i.c,p.c) + low.m * fitness.mc(t+1,k2,i.c,p.c)
      }
      else if (any(p.c!=nlength[-1]) && any(m.c==mlength[-1])) { #If m.c is an integer, use it.
        high.p * fitness.mc(t+1,m.c,i.c,j) + low.p * fitness.mc(t+1,m.c,i.c,j2)
      }
      else { #Neither m.c nor p.c are integers (equation 2.9; Mangel, 2000). 
        high.p * high.m * fitness.mc(t+1,k,i.c,j) + high.p * low.m * fitness.mc(t+1,k2,i.c,j) + low.p * high.m * fitness.mc(t+1,k,i.c,j2) + low.p * low.m * fitness.mc(t+1,k2,i.c,j2)
      }
    }
  return(np.c)
}


####################################################
#################### CONSUMPTION ###################
####################################################

#The main consumption function. Calculates the mass of consumed food and the energy of consumed food based on latitude, time of year, and body mass. 
consumption <- function(bodymass,c,H) {
  C_g <- 0.019610*exp(0.470617*bodymass) #Consumption in grams as a function of body mass. 
  #Bailey Island, Latitudinal Project (n=157; Griffen et al., 2022).
  C_g <- C_g * c #Multiply mass of consumed food by consumption state.
  C_animal <- H * C_g #Calculate the mass of animal tissue consumed. 
  C_algae <- (1-H) * C_g #Calculate the mass of algal tissue consumed. 
  CE_animal <- C_animal * 19.71 #Calculate the energy of consumed animal material, in kJ (19.71 kJ/g for mussel tissue; Griffen, 2014).
  CE_algae <- C_algae * 8.37 #Calculate the energy of consumed algal material, in kJ (8.37 kJ/g for algae; Griffen, 2014).
  C_energy <- CE_animal + CE_algae #Add energy from consumed animal tissue to energy from consumed algal material.
  c_values <- list(C_energy,C_g) #Combine the energy and mass of consumed food into a list so both values can be returned by the function. 
  return(c_values) #Return both the mass and energy of consumed food. 
}


###################################################
###################### FECES ######################
###################################################

#The main feces function. Calculates the daily energy lost to feces based on the mass of food consumed and diet quality. 
#Will base this off standard animal prey (mussel: 19.71 kJ/g) and chondrus (8.37 kJ/g), which was determined
#using calorimetry as reported in Griffen (2014). Assumes that feces is proportionally equivalent to intake in terms of animal and algae.
#We also assume here that fecal energy is the same as food energy (i.e., just undigested food).
feces <- function(C_g) { 
  Feces <- -0.65841*exp(-0.04837*C_g) + 0.80954*exp(0.29079*(1-H)) #Proportional fecal production (Griffen et al., 2015).
  if(Feces>1){Feces<-1} #Some values of F at high consumption are >1. This constrains them to 1.
  Feces <- Feces * C_g #Convert proportional fecal production to absolute mass of feces produced.
  F_animal <- Feces * H * 19.71 #Convert mass of feces to energy of undigested animal tissue. 
  F_algae <- Feces * (1-H) * 8.37 #Convert mass of feces to energy of undigested algal tissue.
  Feces_energy <- F_animal + F_algae #Add energy of animal and algal fecal material to yield total energy of feces. 
  return(Feces_energy)
}


#################################################
############### BREEDING SEASON #################
#################################################

#This function determines whether a certain date falls within the breeding season. 
breeding <- function(juldate,latitude) {
  if(latitude == "N"){ #Maine and new Hampshire (Gulf of Maine)
    if (juldate > 91 || juldate < 275) { #If during the breeding season, ovigerous.  
      ovigerous <- 1
    }
    else { #If not during the breeding season, not ovigerous. 
      ovigerous <- 0
    }
  }
  else { #Locations south of New Hampshire
    if (juldate > 60 || juldate < 275) { #If during the breeding season, ovigerous. 
      ovigerous <- 1
    }
    else { #If not during the breeding season, not ovigerous. 
      ovigerous <- 0
    }
  }
  return(ovigerous)
}



###################################################
################# METABOLISM ######################
###################################################

#This function calculates total daily metabolic costs based on bodymass, time of year, and latitude. 
metabolism <- function(juldate,bodymass,latitude) {
  WaterTemp <- 11.5 + (8.5 * cos(2 * pi / 365 * (juldate - 240))) #From Griffen 2017 and gives water temp in degrees C. 
  AirTempF <- 50 + (20 * cos(2 * pi / 365 * (juldate - 186))) #Using data from weatherspark.com for Hampton Beach, NH air temps throughout year.
  #Fit equation to this trial and error until we obtained the appropriate distribution.
  AirTempC <- (AirTempF - 32) * 5 / 9 #Convert Farenheit air temp to Celcius air temp. These are MEAN temperatures.
  
  freshmass <- bodymass/0.26 #Convert dry body mass to wet mass (Ricciardi & Bourget, 1998).
  R_water <- exp(9.76 + (-0.38 * log(freshmass)) + (0.10 * WaterTemp)) #In nmol/d/g (Jungblut et al. 2018 p. 54).
  R_water <- R_water * freshmass #Convert back to nmol/d (we want total energy used up in metabolism, not just energy per gram).
  R_mmol <- R_water / 1000 #Divide by 1000 to convert from nmol to micromols.
  R_water_nongravid <- R_mmol / 44.64 #Divide by 44.64 because 1 ml 02 is 44.64 micromols of O2 (https://lukemiller.org/index.php/2013/07/o2-conversion/).
  #So this converts to ml O2/d.
  
  R_air_gravid <- 24*(0.107595 + bodymass*0.150346)
  #ml O2 consumed per day for gravid females as a function of body mass. 
  #Odiorne Pt and Bailey Island, July, 2022 (n=280).
  
  R_air_nongravid <- 24*(0.0862 + bodymass*0.16135)
  #ml O2 consumed per day for non-breeding females as a function of body mass. 
  #Odiorne Pt and Bailey Island, July, 2022 (n=138).
  
  gravid_cost <- R_air_gravid/R_air_nongravid #Proportional cost of being gravid in air. 
  
  R_water_gravid <- R_water_nongravid * gravid_cost #Multiply nongravid metabolic costs in water by the proportional cost of being gravid in air. 
  #We are assuming here that being gravid exacts the same proportional cost in water as in air. 
  
  ovigerous <- breeding(juldate,latitude)
  if (ovigerous == 1){ #If during the breeding season, account for higher metabolic costs of ovigery. 
    R_air <- R_air_gravid
    R_water <- R_water_gravid
  }
  else { #If outside the breeding season.
    R_air <- R_air_nongravid
    R_water <- R_water_nongravid
  }
  
  R_energy_air <- (R_air * 19.7)/ 1000 #Are 19.7 J/ml O2 metabolized (p. 5 of Karasov and Martinez del Rio 2007), so this converts from ml O2 to J consumed.
  #Then divides by 1000 to yield kJ.
  
  R_energy_water <- (R_water * 19.7)/ 1000 #Are 19.7 J/ml O2 metabolized (p. 5 of Karasov and Martinez del Rio 2007), so this converts from ml O2 to J consumed.
  #Then divides by 1000 to yield kJ.
  
  Metabolism_energy <- (t_submerged*R_energy_water) + ((1-t_submerged)*R_energy_air) #Multiply water and air respiration by the proportion of time submerged and emerged, respectively.
  #Total energy lost to metabolism; air + water.
  
  return(Metabolism_energy)
}


###################################################
################# METABOLISM (Monte Carlo) ######################
###################################################

#This function calculates total daily metabolic costs based on bodymass, time of year, and latitude for the Monte Carlo Simulations. 
metabolism.mc <- function(juldate,bodymass,latitude) {
  WaterTemp <- 11.5 + (8.5 * cos(2 * pi / 365 * (juldate - 240))) #From Griffen 2017 and gives water temp in degrees C. 
  AirTempF <- 50 + (20 * cos(2 * pi / 365 * (juldate - 186))) #Using data from weatherspark.com for Hampton Beach, NH air temps throughout year.
  #Fit equation to this trial and error until we obtained the appropriate distribution.
  AirTempC <- (AirTempF - 32) * 5 / 9 #Convert Farenheit air temp to Celcius air temp. These are MEAN temperatures.
  
  freshmass <- bodymass/0.26 #Convert dry body mass to wet mass (Ricciardi & Bourget, 1998).
  R_water <- exp(9.76 + (-0.38 * log(freshmass)) + (0.10 * WaterTemp)) #In nmol/d/g (Jungblut et al. 2018 p. 54).
  R_water <- R_water * freshmass #Convert back to nmol/d (we want total energy used up in metabolism, not just energy per gram).
  R_mmol <- R_water / 1000 #Divide by 1000 to convert from nmol to micromols.
  R_water_nongravid <- R_mmol / 44.64 #Divide by 44.64 because 1 ml 02 is 44.64 micromols of O2 (https://lukemiller.org/index.php/2013/07/o2-conversion/).
  #So this converts to ml O2/d.
  
  R_air_gravid <- 24*(0.107595 + bodymass*0.150346)
  #ml O2 consumed per day for gravid females as a function of body mass. 
  #Odiorne Pt and Bailey Island, July, 2022 (n=280).
  
  R_air_nongravid <- 24*(0.0862 + bodymass*0.16135)
  #ml O2 consumed per day for non-breeding females as a function of body mass. 
  #Odiorne Pt and Bailey Island, July, 2022 (n=138).
  
  gravid_cost <- R_air_gravid/R_air_nongravid #Proportional cost of being gravid in air. 
  
  R_water_gravid <- R_water_nongravid * gravid_cost #Multiply nongravid metabolic costs in water by the proportional cost of being gravid in air. 
  #We are assuming here that being gravid exacts the same proportional cost in water as in air. 
  
  ovigerous <- breeding(juldate,latitude)
  if (ovigerous == 1){ #If during the breeding season, account for higher metabolic costs of ovigery. 
    R_water <- R_water_gravid
  }
  else { #If outside the breeding season.
    R_water <- R_water_nongravid
  }
  
  R_energy_water <- (R_water * 19.7)/ 1000 #Are 19.7 J/ml O2 metabolized (p. 5 of Karasov and Martinez del Rio 2007), so this converts from ml O2 to J consumed.
  #Then divides by 1000 to yield kJ.
  
  Metabolism_energy <- R_energy_water
  #Total energy lost to metabolism. Only includes water metabolism because individuals in the experiment were submerged the vast majority of the time. 
  
  return(Metabolism_energy)
}


########################################################
################# TIME CONVERSION ######################
########################################################

#Calculates month from julian date. Used in the Monte Carlo simulations. 
t_calculator <- function(juldate,bodymass) {
  
  #initial calculations
  if(juldate > 90 && juldate < 121) {
    t <- 1
  }
  if(juldate > 120 && juldate < 152) {
    t <- 2
  }
  if(juldate > 151 && juldate < 182) {
    t <- 3
  }
  if(juldate > 181 && juldate < 213) {
    t <- 4
  }
  if(juldate > 212 && juldate < 244) {
    t <- 5
  }
  if(juldate > 243 && juldate < 274) {
    t <- 6
  }
  
  #modify based on body size
  if (bodymass <= 1) {
    t <- t #first year of maturity
  }
  
  if (bodymass > 1 && bodymass <= 2) {
    t <- t + 12 #second year of maturity
  }
  
  if(bodymass >2) {
    t <- t + 24 #third year of maturity
  }
  
  return(t)
  
}