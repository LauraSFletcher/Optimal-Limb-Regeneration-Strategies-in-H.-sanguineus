########### STRATEGY FUNCTIONS ##########

#Strategy specific functions to be called within the main fecundity function.
#Determines how excess energy is allocated during each monthly time step. 


########################################
############# STRATEGY ONE #############
########################################

#All excess energy to reproduction. 

strategy1 <- function(E,P_energy) {
  rep_energy <- E #For strategy 1, all excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  return(P_energy)
}


########################################
############# STRATEGY TWO #############
########################################

#2/3 excess energy to reproduction, 1/3 excess energy to growth.

strategy2 <- function(E,P_energy,bodymass) {
  rep_energy <- (2/3)*E #For strategy 2, 2/3 excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  G_energy <- E/3 #For strategy 2, 1/3 excess energy goes to growth.
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
  values <- list(P_energy,bodymass) #Combine body mass and energy into a list to return both values. 
  return(values)
}


##########################################
############# STRATEGY THREE #############
##########################################

#2/3 excess energy to reproduction, 1/3 excess energy to regeneration.

strategy3 <- function(E,P_energy,regeneration) {
  rep_energy <- (2/3)*E #For strategy 3, 2/3 excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  R_energy <- E/3 #For strategy 3, 1/3 excess energy goes to regeneration.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J. 
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue. 
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(P_energy, regeneration) #Add regenerated mass and energy to a list to return both values. 
  return(values)
}


#########################################
############# STRATEGY FOUR #############
#########################################

#1/3 excess energy to reproduction, 2/3 excess energy to growth. 

strategy4 <- function(E,P_energy,bodymass) {
  rep_energy <- E/3 #For strategy 4, 1/3 excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  G_energy <- (2/3)*E #For strategy 4, 2/3 excess energy goes to growth.
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #Convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If already at the maximum body size, then growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  values <- list(P_energy,bodymass) #Add energy and body mass to a list to return both values. 
  return(values)
}


#########################################
############# STRATEGY FIVE #############
#########################################

#1/3 excess energy to reproduction, 2/3 excess energy to regeneration. 

strategy5 <- function(E,P_energy,regeneration) {
  rep_energy <- E/3 #For strategy 5, 1/3 excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  R_energy <- (2/3)*E #For strategy 5, 2/3 excess energy goes to regeneration.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(P_energy,regeneration) #Add energy and regenerated mass to a list to return both values. 
  return(values)
}


########################################
############# STRATEGY SIX #############
########################################

#1/3 excess energy to reproduction, 1/3 excess energy to growth, 1/3 excess energy to regeneration. 

strategy6 <- function(E,P_energy,bodymass,regeneration) {
  rep_energy <- E/3 #For strategy 6, 1/3 excess energy goes to hepato/ovary for later reproduction.
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  R_energy <- E/3 #For strategy 6, 1/3 excess energy goes to regeneration.
  G_energy <- E/3 #For strategy 6, 1/3 excess energy goes to growth.
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #Convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If already at the maximum body size, then growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(P_energy,bodymass,regeneration) #Add energy, body mass, and regenerated mass to a list to return all three values. 
  return(values)
}


##########################################
############# STRATEGY SEVEN #############
##########################################

#All excess energy to growth. 

strategy7 <- function(E,bodymass) {
  G_energy <- E #For strategy 7, all excess energy goes to growth.
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to South Carolina). 
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If body mass is already at maximum value (3 g), growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  return(bodymass)
}


##########################################
############# STRATEGY EIGHT #############
##########################################

#All excess energy to regeneration. 

strategy8 <- function(E,regeneration) {
  R_energy <- E #For strategy 8, all excess energy goes to regeneration.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  return(regeneration)
}


#########################################
############# STRATEGY NINE #############
#########################################

#1/3 excess energy to regeneration, 2/3 excess energy to growth. 

strategy9 <- function(E,bodymass,regeneration) {
  R_energy <- E/3 #For strategy 9, 1/3 excess energy goes to regeneration.
  G_energy <- (2/3)*E #For strategy 9, 2/3 excess energy goes to growth.
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to South Carolina). 
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If body mass is already at maximum value (3 g), growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(bodymass,regeneration) #Add body mass and regenerated mass to a list to return both values. 
  return(values)
}


########################################
############# STRATEGY TEN #############
########################################

#2/3 excess energy to regeneratio, 1/3 excess energy to growth. 

strategy10 <- function(E,bodymass,regeneration) {
  R_energy <- (2/3)*E #For strategy 10, 2/3 excess energy goes to regeneration.
  G_energy <- E/3 #For strategy 10, 1/3 excess energy goes to growth.
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to South Carolina). 
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If body mass is already at maximum value (3 g), growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(bodymass,regeneration) #Add body mass and regenerated mass to a list to return both values. 
  return(values)
}


###################################################
############# FIXED STRATEGY ANALYSIS #############
###################################################

#Function to divide energy based on the fixed energy allocation strategy in the Monte Carlo simulations. 

fixedstrat <- function(k,E,P_energy,bodymass,regeneration) {
  rep_energy <- E*(1-k) #All energy not use for regeneration/growth goes to reproduction. 
  P_energy <- P_energy + rep_energy #Add energy to hepato/ovary.
  R_energy <- E*(k/2) #Half of k goes to regeneration, half to growth. 
  G_energy <- E*(k/2) 
  #Calculating mass gained from growth energy.
  G_metabolism <- 7150 #J/g of tissue synthesized.
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature. 
  G_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  G_total <- G_metabolism + G_tissue #Total cost of synthesizing muscle tissue in J/g. 
  G_energy <- G_energy * 1000 #Convert growth energy from kJ to J.
  growth <- G_energy / G_total #Calculate total mass of synthesized tissue. 
  if (bodymass >= massmax) { #If already at the maximum body size, then growth is not possible. 
    growth <- 0
  }
  bodymass <- bodymass + growth #Add new mass of muscle tissue to overall body mass.
  #Calculating mass gained from regeneration energy.
  R_metabolism <- 7150 #J/g of tissue synthesized (assuming the same metabolic cost as body tissue).
  #In Carcinus maenas, 7150 J/g. This is the only valuable available in the literature.
  R_tissue <- 75613.4 #J/g of muscle tissue from calorimetry (n=10, samples collected from Maine down to North Carolina).
  R_total <- R_metabolism + R_tissue #Total cost of synthesizing limb tissue in J/g. 
  R_energy <- R_energy * 1000 #Convert regeneration energy from kJ to J.
  new_regen <- R_energy / R_total #Mass of newly regenerated tissue.
  regeneration <- regeneration + new_regen #Total mass of regenerated tissue so far this month.
  values <- list(P_energy,bodymass,regeneration) #Add energy, body mass, and regenerated mass to a list to return all three values. 
  return(values)
}

