### MODEL PARAMETERIZATION ###

##Consumption; Latitudinal Project

rm(list = ls()) ##resets R so that nothing transfers over from past iterations
Latitude <- read.csv("~/Hemigrapsus Latitudinal Project.csv", header = T) #import data. Note: user will have to edit file path. 
attach(Latitude) #attach the file so column names can be called independently of file name
names(Latitude) #list column names in file

#Consumption; Bailey Island, ME
NH <- subset(Latitude, CollectionSite=="NH")
#n=157

summary(nls(Gut~a*exp(b*Body),start=c(a=0.1,b=2),data=NH))

C_g <- 0.019610*exp(0.470617*bodymass)

summary(lm(P~Body,data=Latitude))

#P_mass <- 0.10656*bodymass - 0.013258 


##Metabolism; July 2022, Bailey Island, ME and Odiorne Pt, NH

rm(list = ls()) ##resets R so that nothing transfers over from past iterations
detach(Maine)
Maine <- read.csv("~/Maine Data.csv", header = T) #import data. Note: user will have to edit file path. 
attach(Maine) #attach the file so column names can be called independently of file name
names(Maine) #list column names in file

Maine <- subset(Maine, Sex == "F" & Species == "H.s") #remove Carcinus and male individuals from dataset
#n=423

#split dataset into gravid and nongravid females
gravid_females <- subset(Maine,Gravid=="1")
#n=280
nongravid_females <- subset(Maine,Gravid=="0")
#n=138

#MR for gravid females
summary(lm(MR~Body, data=gravid_females))

R_air <- 24*(0.107595 + bodymass*0.150346)

#MR for nongravid females
summary(lm(MR~Body, data=nongravid_females))

R_air <- 24*(0.0862 + bodymass*0.16135)


Maine <- subset(Maine,Body > 0) #n = 420

summary(lm(Body~WetMass,data=Maine))

plot(Body~WetMass,data=Maine)
abline(lm(Body~WetMass))

Body <- Maine$Body
WetMass <- as.numeric(Maine$WetMass)


summary(lm(Body~WetMass))

#bodymass <- 0.32572*wetmass -0.138044









