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
population <-read.csv("Populations/ADVANCE - MTHood2018 - F.csv")
population_clean <- build_population(population, PopulationVariables, GlobalVars)
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

#Base Case analysis - Trial effects and default treatment effect duration
set.seed(1)
start.time <- Sys.time()

results1det <- run_model(population_clean, 
                            data, 
                            20, 
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
                      20, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2det, "Results/MachineLearningNoInterventiondet.csv")

##PSA
GlobalVars["run_psa","Value"] <- TRUE
set.seed(1)
start.time <- Sys.time()

results_intv_psa <- run_model(population_clean, 
                         data, 
                         20, 
                         "MLexample", 
                         GlobalVars,
                         random_numbers,
                         LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_intv_psa, "Results/MachineLearningExampleInterventionPSA.csv")
rm(results_intv_psa)

set.seed(1)
start.time <- Sys.time()
results_nointv_psa <- run_model(population_clean, 
                         data, 
                         20, 
                         "baseline", 
                         GlobalVars,
                         random_numbers,
                         LifeTables)

end.time <- Sys.time()
end.time - start.time

write.csv(results_nointv_psa, "Results/MachineLearningNoInterventionPSA.csv")
rm(results_nointv_psa)

##2nd set of PSAs

set.seed(1)
start.time <- Sys.time()

results_intv_psa2 <- run_model(population_clean, 
                              data2, 
                              20, 
                              "MLexample", 
                              GlobalVars,
                              random_numbers,
                              LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_intv_psa2, "Results/MachineLearningExampleInterventionPSA2.csv")
rm(results_intv_psa2)

set.seed(1)
start.time <- Sys.time()

results_nointv_psa2 <- run_model(population_clean, 
                                data2, 
                                20, 
                                "baseline", 
                                GlobalVars,
                                random_numbers,
                                LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_nointv_psa2, "Results/MachineLearningNoInterventionPSA2.csv")
rm(results_nointv_psa2)

##3rd set of PSAs

set.seed(1)
start.time <- Sys.time()

results_intv_psa3 <- run_model(population_clean, 
                              data3, 
                              20, 
                              "MLexample", 
                              GlobalVars,
                              random_numbers,
                              LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_intv_psa3, "Results/MachineLearningExampleInterventionPSA3.csv")
rm(results_intv_psa3)

set.seed(1)
start.time <- Sys.time()
results_nointv_psa3 <- run_model(population_clean, 
                                data3, 
                                20, 
                                "baseline", 
                                GlobalVars,
                                random_numbers,
                                LifeTables)
end.time <- Sys.time()
end.time - start.time
write.csv(results_nointv_psa3, "Results/MachineLearningNoInterventionPSA3.csv")
rm(results_nointv_psa3)

##4th set of PSAs

set.seed(1)
start.time <- Sys.time()

results_intv_psa4 <- run_model(population_clean, 
                              data4, 
                              20, 
                              "MLexample", 
                              GlobalVars,
                              random_numbers,
                              LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_intv_psa4, "Results/MachineLearningExampleInterventionPSA4.csv")
rm(results_intv_psa4)

set.seed(1)
start.time <- Sys.time()

results_nointv_psa4 <- run_model(population_clean, 
                                data4, 
                                20, 
                                "baseline", 
                                GlobalVars,
                                random_numbers,
                                LifeTables)
end.time <- Sys.time()
end.time - start.time
write.csv(results_nointv_psa4, "Results/MachineLearningNoInterventionPSA4.csv")
rm(results_nointv_psa4)

##5th set of PSAs

set.seed(1)
start.time <- Sys.time()

results_intv_psa5 <- run_model(population_clean, 
                              data5, 
                              20, 
                              "MLexample", 
                              GlobalVars,
                              random_numbers,
                              LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results_intv_psa5, "Results/MachineLearningExampleInterventionPSA5.csv")
rm(results_intv_psa5)

set.seed(1)
start.time <- Sys.time()

results_nointv_psa5 <- run_model(population_clean, 
                                data5, 
                                20, 
                                "baseline", 
                                GlobalVars,
                                random_numbers,
                                LifeTables)
end.time <- Sys.time()
end.time - start.time
write.csv(results_nointv_psa5, "Results/MachineLearningNoInterventionPSA5.csv")
rm(results_nointv_psa5)
