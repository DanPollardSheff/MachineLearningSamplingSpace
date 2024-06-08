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



##'@param population_, is the popualtion matrix
##'@param parameter_, is the row of hte parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return A1c is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_A1c <- function(population_, parameter_, endtime_){
  
  #set up a matrix to store the results
  A1c <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  A1c[,1] <- population_[,"ID"]
  #Make the second column the baseline HbA1c
  A1c[,2:(endtime_ + 2)] <- population_[,"HBA"]
  
  colnames(A1c) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(A1c)[1] <- c("ID") #overwrite the first column with ID
  
  return(A1c)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return SBP is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_SBP <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  SBP <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  SBP[,1] <- population_[,"ID"]
  #Make the second column the baseline SBP
  SBP[,2:(endtime_ + 2)] <- population_[,"SBP"]
  
  #name the first column ID
  colnames(SBP) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(SBP)[1] <- c("ID") #overwrite the first column with ID
  
  return(SBP)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_LDL <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  LDL <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  LDL[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  LDL[,2:(endtime_+2)] <- population_[,"LDL"]
  
  #name the first column ID
  colnames(LDL) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(LDL)[1] <- c("ID") #overwrite the first column with ID
  
  return(LDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_HDL <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  HDL <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  HDL[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HDL[,2:(endtime_+2)] <- population_[,"HDL"]
  
  #name the first column ID
  colnames(HDL) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(HDL)[1] <- c("ID") #overwrite the first column with ID
  
  return(HDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return BMI is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_BMI <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  BMI <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  BMI[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  BMI[,2:(endtime_+2)] <- population_[,"BMI"]

  #name the first column ID
  colnames(BMI) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(BMI)[1] <- c("ID") #overwrite the first column with ID
  
  return(BMI)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HEARTR is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_HEARTR       <- function(population_, parameter_, endtime_){

  #set up a matrix to store the results
  HEARTR <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  HEARTR[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HEARTR[,2:(endtime_+2)] <- population_[,"HEART_R"]
  
  #name the first column ID
  colnames(HEARTR) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(HEARTR)[1] <- c("ID") #overwrite the first column with ID
  
  return(HEARTR)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return WBC is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_WBC          <- function(population_, parameter_, endtime_){

  #set up a matrix to store the results
  WBC <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  WBC[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  WBC[,2:(endtime_+2)] <- population_[,"WBC"]
  #name the first column ID
  colnames(WBC) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(WBC)[1] <- c("ID") #overwrite the first column with ID
  
  return(WBC)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HAEM is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Constant_trajectory_HAEM         <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  HAEM <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  HAEM[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HAEM[,2:(endtime_+2)] <- population_[,"HAEM"]
  
  #name the first column ID
  colnames(HAEM) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(HAEM)[1] <- c("ID") #overwrite the first column with ID
  
  return(HAEM)
  
}
