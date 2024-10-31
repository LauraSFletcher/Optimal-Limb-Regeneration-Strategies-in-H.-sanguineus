# Optimal-Limb-Regeneration-Strategies-in-H.-sanguineus
This repository contains all the data files needed to run a dynamic state variable model used to predict optimal energy allocation strategies in crabs. 

#MODEL PURPOSE: The dynamic state variable model (DSVM) determines optimal energy allocation strategies for an adult 
female crab based on five state variables (body mass, energy reserves, age in months since maturity, injury and 
regeneration status, and consumption amount) via backward iteration and linear interpolation. Monte Carlo simulations
of individuals determine the fitness of individuals following the optimal energy allocation strategy and fixed energy 
allocation strategies, as well as the fitness of individuals following the optimal strategies as a function of diet
quality, time of year, body size, and injury severity. 

#FILE DESCRIPTIONS: There are six separate files associated with the dynamic state variable model. The main model file is titled "Optimal Injury Recovery.R". Run 
the code in this file for the complete graphical model output. Before running the model, be sure to set your working directory to "Source File
Location." In this file, the fitness and best strategy arrays are created and filled with values as the model runs. The model loops
through each possible value for the five state variables, running backwards through time, and calls up 
the fecundity function. This function, located in the file "Fecundity Function.R," calculates daily bioenergetics and the total fitness 
that can be produced for each energy allocation strategy, which are themselves defined in the file "Strategy Functions.R." The main model then
selects the highest fitness value returned by the fecundity function and indexes this in the fitness array. The optimal strategy (1-10) is recorded
in the best strategy array and is graphically represented by colorized heatmaps produced by the function located in the "Graph.R" file. Each of the
ten strategies receives its own color. See the "Graph.R" file for a color key. 

The fecundity function ("Fecundity Function.R" file) itself depends on three other files: "Functions.R", "Variables.R", and "Strategy
Functions.R". In the Functions file, you will find all the bioenergetics functions, including the metabolism, consumption, and feces functions. 
This function also includes the mortality function (which alters mortality based on body mass) the bounds functions (which ensure that
computer energy and body mass values stay within the minimum and maximum values), the fitness function (which pulls future fitness values
from the fitness array during linear interpolation), and the linear interpolation function (which calculates future fitness). The Variables
file includes all variables which can be manually changed prior to running the model. These include latitude, injury severity, the energy
required to produce a single egg, diet quality (the percentage of diet comprised of animal tissue vs. algal tissue), and injured mortality
(a value which determines how much higher mortality for injured individuals is compared to uninjured individuals). This file also includes
global variables which are used throughout the model's files and do not change, such as vectors describing the range of values of the state
variables, which are used in the main model for loops. Finally, the Strategy Functions file contains strategy-specific bioenergetics
code. Each function in this file, one for each of the ten strategies, calculates how body mass, energy reserves, and regeneration should change
on a daily basis according to their respective energy allocation strategy.

There are three additional files associated with the Monte Carlo simulations: "Fixed vs. Optimal Monte Carlo.R", "Fecundity Function (Monte Carlo).R", 
and "Variables (Monte Carlo).R." Run the first of these files to obtain the Monte Carlo simulation results, including figures and .csv files for 
model-predicted reproductive output. The Monte Carlo variables file differs from the DSVM variables file because it does not include injury severity 
(or, as it appears in the code, "missing_limbs") and diet quality, which are determined within the code of the diet quality and seasonal, body size, 
and injury severity Monte Carlo simulations for each simulated individual. 





