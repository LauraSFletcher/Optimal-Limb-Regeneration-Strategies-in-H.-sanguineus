rm(list = ls()) #clear memory
detach(Latitudinal)
Latitudinal <- read.csv("~/Hemigrapsus Latitudinal Project.csv", header = T) #import data. Note: user will have to edit file path. 
attach(Latitudinal) #attach the file so column names can be called independently of file name
names(Latitudinal) #list column names in file

plot(P~Body,data=Latitudinal)
abline(0,0.17,col="purple") #max line (ignoring outliers) (To determine Pmax)
abline(0,0.03,col="green") #min line (To determine Pmin)


energy <- subset(Latitudinal,Penergy_kJ>0) #n = 243

#p_max <- 0.17*bodymass #Maximum mass of hepatopancreas tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022)
#p_min <- (0.03*bodymass #Minimum mass of hepato/ovary tissue (taken from samples collected throughout the invaded range in 2020, n = 799; Griffen et al. 2022)
#P_joules <- 32.1672-(1.0387*bodymass) #kJ/g of hepato/ovary tissue (taken from calorimetry data, n = 245; to be published elsewhere)
#P_max <- p_max * P_joules #Maximum energy of hepato/ovary tissue 
#P_min <- p_min * P_joules #Minimum energy of hepato/ovary tissue 

summary(lm(P_kJ_g~Body, data=Latitudinal))
plot(P_kJ_g~Body, data=Latitudinal)
abline(lm(P_kJ_g~Body, data=Latitudinal))

#P_joules <- 32.1672-(1.0387*bodymass) #kJ/g

eggs <- subset(Latitudinal, Eenergy>0) #n = 213 clutches

E_energy <- as.numeric(eggs$Eenergy)
Body <- eggs$Body
E_kJ <- E_energy/(eggs$Eggs)

summary(lm(E_kJ~Body))

#E_kJ <- 32.9725-(2.388*Body)

plot(Eenergy~Body,data=eggs)
abline(0.5,3,col="red")

#max_eggs <- (3 * bodymass) + 0.5

#Starting energy for monte carlo simulations
gravid <- subset(energy,Gravid==1) #n = 83

summary(lm(Penergy_kJ~Body,data=gravid))

#P_energy <- (2.8544*bodymass) + 1.1748
