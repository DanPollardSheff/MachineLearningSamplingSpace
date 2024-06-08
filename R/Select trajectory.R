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



##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@param GlobalVars_, is the matrix of global model control options
##'@return A1c is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_A1c <- function(population_, parameter_, endtime_, GlobalVars_){
  
 ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    A1c <- Constant_trajectory_A1c(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    A1c <- UKPDS_90_contrisk_A1c(population_,parameter_,endtime_)
  }
  
  return(A1c)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@param GlobalVars_, is the matrix of global model control options
##'@return SBP is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_SBP <- function(population_, parameter_, endtime_,GlobalVars_){
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    SBP <- Constant_trajectory_SBP(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    SBP <- UKPDS_90_contrisk_SBP(population_,parameter_,endtime_)
  }
  
  return(SBP)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_LDL <- function(population_, parameter_, endtime_,GlobalVars_){
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    LDL <- Constant_trajectory_LDL(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    LDL <- UKPDS_90_contrisk_LDL(population_,parameter_,endtime_)
  }
  
  return(LDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_HDL <- function(population_, parameter_, endtime_, GlobalVars_){
  
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    HDL <- Constant_trajectory_HDL(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    HDL <- UKPDS_90_contrisk_HDL(population_,parameter_,endtime_)
  }
  
  return(HDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return BMI is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_BMI <- function(population_, parameter_, endtime_, GlobalVars_){
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    BMI <- Constant_trajectory_BMI(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    BMI <- UKPDS_90_contrisk_BMI(population_,parameter_,endtime_)
  }
  
  return(BMI)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HEARTR is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_HEARTR       <- function(population_, parameter_, endtime_, GlobalVars_){
  
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    HEARTR <- Constant_trajectory_HEARTR(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    HEARTR <- UKPDS_90_HEARTR(population_,parameter_,endtime_)
  }
  
  return(HEARTR)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return WBC is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_WBC          <- function(population_, parameter_, endtime_, GlobalVars_){
  
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    WBC <- Constant_trajectory_WBC(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    WBC <- UKPDS_90_WBC(population_,parameter_,endtime_)
  }
  
  return(WBC)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HAEM is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

Select_trajectory_HAEM         <- function(population_, parameter_, endtime_, GlobalVars_){
  
  ##If a constant trajectory is selected use this
  if(GlobalVars_["Trajectory Source","Value"]=="Constant"){
    HAEM <- Constant_trajectory_HAEM(population_,parameter_,endtime_)
  }else{#otherwise use UKPDS 90 - default
    HAEM <- UKPDS_90_HAEM(population_,parameter_,endtime_)
  }
  
  
  return(HAEM)
  
}
