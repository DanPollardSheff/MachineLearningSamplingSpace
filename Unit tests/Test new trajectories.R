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


#Load up the model files

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
population_ <- build_population(population, PopulationVariables, GlobalVars)

parameters_ <- data[1,]
endtime_ <- 8

##Check that the selection algorithm gives the same answers as the underlying trajectory
GlobalVars["Trajectory Source", "Value"] <- "UKPDS90"

HBA1c_underlying  <- UKPDS_90_contrisk_A1c(population_,parameters_,endtime_)
BMI_underlying    <- UKPDS_90_contrisk_BMI(population_,parameters_,endtime_)
SBP_underlying    <- UKPDS_90_contrisk_SBP(population_,parameters_,endtime_)
HDL_underlying    <- UKPDS_90_contrisk_HDL(population_,parameters_,endtime_)
LDL_underlying    <- UKPDS_90_contrisk_LDL(population_,parameters_,endtime_)
HEARTR_underlying <- UKPDS_90_HEARTR(population_,parameters_,endtime_)
WBC_underlying    <- UKPDS_90_WBC(population_,parameters_,endtime_)
HAEM_underlying   <- UKPDS_90_HAEM(population_,parameters_,endtime_)

HBA1c_underlying_UKPDS90  <- Select_trajectory_A1c(population_,parameters_,endtime_,GlobalVars)
BMI_underlying_UKPDS90    <- Select_trajectory_BMI(population_,parameters_,endtime_,GlobalVars)
SBP_underlying_UKPDS90    <- Select_trajectory_SBP(population_,parameters_,endtime_,GlobalVars)
HDL_underlying_UKPDS90    <- Select_trajectory_HDL(population_,parameters_,endtime_,GlobalVars)
LDL_underlying_UKPDS90    <- Select_trajectory_LDL(population_,parameters_,endtime_,GlobalVars)
HEARTR_underlying_UKPDS90 <- Select_trajectory_HEARTR(population_,parameters_,endtime_,GlobalVars)
WBC_underlying_UKPDS90    <- Select_trajectory_WBC(population_,parameters_,endtime_,GlobalVars)
HAEM_underlying_UKPDS90   <- Select_trajectory_HAEM(population_,parameters_,endtime_,GlobalVars)


#Change the source to the constant trajectory
GlobalVars["Trajectory Source", "Value"] <- "Constant"

HBA1c_underlying_Cons  <- Select_trajectory_A1c(population_,parameters_,endtime_,GlobalVars)
BMI_underlying_Cons    <- Select_trajectory_BMI(population_,parameters_,endtime_,GlobalVars)
SBP_underlying_Cons    <- Select_trajectory_SBP(population_,parameters_,endtime_,GlobalVars)
HDL_underlying_Cons    <- Select_trajectory_HDL(population_,parameters_,endtime_,GlobalVars)
LDL_underlying_Cons    <- Select_trajectory_LDL(population_,parameters_,endtime_,GlobalVars)
HEARTR_underlying_Cons <- Select_trajectory_HEARTR(population_,parameters_,endtime_,GlobalVars)
WBC_underlying_Cons    <- Select_trajectory_WBC(population_,parameters_,endtime_,GlobalVars)
HAEM_underlying_Cons   <- Select_trajectory_HAEM(population_,parameters_,endtime_,GlobalVars)


##Test whether the objects are identical, want an integer(0) output
HBA1c_underlying_Cons
BMI_underlying_Cons
SBP_underlying_Cons
HDL_underlying_Cons
LDL_underlying_Cons
HEARTR_underlying_Cons
WBC_underlying_Cons
HAEM_underlying_Cons
