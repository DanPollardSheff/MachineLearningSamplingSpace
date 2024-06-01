#    Embedding RCT Health Economic Analysis using the Sheffield Type 2 Diabetes Treatment Model - version 3
#    Copyright (C) 2023   Pollard, Pidd, Breeze, Brennan, Thomas

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License along
#    with this program; if not, write to the Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#    Contact person: Dan Pollard, Email: d.j.pollard@sheffield.ac.uk, 
#    Address: Regent Court, 30 Regent Court, Sheffield, United Kingdom, S1 4DA


library(MASS)
library(VGAM)
library(doParallel)
library(parallel)
library(dplyr)
set.seed(429)

#Create the Global options matrix
source("Global Options.R")
#Load in population and data files
source("all_model_files.R")


####build the population
population <-read.csv("Populations/POPULATION.csv")
population_clean <- build_population(population, PopulationVariables, GlobalVars)
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

#Base Case analysis - Trial effects and default treatment effect duration
set.seed(1)
start.time <- Sys.time()

results1det <- run_model(population_clean, 
                            data, 
                            50, 
                            "MLexample", 
                            GlobalVars,
                            random_numbers,
                            LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1det, "Results/MachineLearningExampleInterventiondet.csv")
set.seed(1)
results2det <- run_model(population_clean, 
                      data, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2det, "Results/MachineLearningNoInterventiondet.csv")

##PSA
GlobalVars["run_psa","Value"] <- TRUE
set.seed(1)
start.time <- Sys.time()

results1psa <- run_model(population_clean, 
                         data, 
                         50, 
                         "MLexample", 
                         GlobalVars,
                         random_numbers,
                         LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1psa, "Results/MachineLearningExampleInterventionPSA.csv")
set.seed(1)
results2psa <- run_model(population_clean, 
                         data, 
                         50, 
                         "baseline", 
                         GlobalVars,
                         random_numbers,
                         LifeTables)

write.csv(results2psa, "Results/MachineLearningNoInterventionPSA.csv")
