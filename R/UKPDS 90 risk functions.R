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
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_smo is the matrix giving the probability of being a smoker next 
##'year from UKPDS 90

UKPDS_90_smo          <- function(population_, parameter_, alive_){
  
  prob_smo <- matrix(NA, nrow = length(population_[, "FEMALE"]),
                     ncol = 1)
  
  FV <- parameter_[,"SMO_UKPDS90_CONS"]+
    parameter_[,"SMO_UKPDS90_FEMALE"]*population_[,"FEMALE"][alive_]+
    parameter_[,"SMO_UKPDS90_AgeDiag"]*
    (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameter_[,"SMO_UKPDS90_SMOlastyear"]*population_[,"SMO"][alive_]+
    parameter_[,"SMO_UKPDS90_SMODiag"]*population_[,"SMO_0"][alive_]+ 
    parameter_[,"SMO_UKPDS90_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1)
  
  prob_smo <- 1/(1+exp(-FV))
  
  rm(FV)
  
  return(prob_smo)

}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_MicAlb is the matrix giving the probability of having micro or
##'macro albuminuria next year from UKPDS 90

UKPDS_90_MICALB       <- function(population_, parameter_, alive_){
  
  MicAlb_start <- exp(parameter_[,"MICALB_CONS"]+
                        parameter_[,"MICALB_FEMALE"]*population_[,"FEMALE"][alive_]+
                        parameter_[,"MICALB_AgeDiag"]*
                             (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                        parameter_[,"MICALB_SMO"]*population_[,"SMO"][alive_]+
                        parameter_[,"MICALB_SBP_div_10"]*
                             (population_[,"SBP"][alive_]/10)+
                        parameter_[,"MICALB_HbA1c"]*population_[,"HBA"][alive_]+
                        parameter_[,"MICALB_HDL_mult_10"]*
                            (population_[,"HDL"][alive_]*10)+
                        parameter_[,"MICALB_BMI"]*
                        population_[,"BMI"][alive_])*(population_[,"DIAB_DUR"][alive_]^parameter_[,"MICALB_shape"])
  
  MicAlb_end <- exp(parameter_[,"MICALB_CONS"]+
                      parameter_[,"MICALB_FEMALE"]*population_[,"FEMALE"][alive_]+
                      parameter_[,"MICALB_AgeDiag"]*
                      (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                      parameter_[,"MICALB_SMO"]*population_[,"SMO"][alive_]+
                      parameter_[,"MICALB_SBP_div_10"]*
                      (population_[,"SBP"][alive_]/10)+
                      parameter_[,"MICALB_HbA1c"]*population_[,"HBA"][alive_]+
                      parameter_[,"MICALB_HDL_mult_10"]*
                      (population_[,"HDL"][alive_]*10)+
                      parameter_[,"MICALB_BMI"]*
                      population_[,"BMI"][alive_])*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"MICALB_shape"])
  
 prob_MicAlb <- 1 - exp(MicAlb_start - MicAlb_end)
 #Make MMALB impossible for people with PVD history
 prob_MicAlb <- ifelse(population_[,"MMALB_H"][alive_]==1, 0, prob_MicAlb)
 
 #remove derived variables that aren't returned
 rm(MicAlb_start, MicAlb_end)
 
 return(prob_MicAlb)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_PVD is the matrix giving the probability of having PVD
##' next year from UKPDS 90

UKPDS_90_PVD          <- function(population_, parameter_, alive_){
  
  PVD_start <- exp(parameter_[,"PVD_Cons"]+
                     parameter_[,"PVD_AgeDiag"]*
                     (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                     parameter_[,"PVD_smoker"]*population_[,"SMO"][alive_]+
                     parameter_[,"PVD_SBP_div10"]*
                     (population_[,"SBP"][alive_]/10)+
                     parameter_[,"PVD_HbA1c"]*population_[,"HBA"][alive_]+
                     parameter_[,"PVD_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"PVD_LDL_mult10"]*
                     (population_[,"LDL"][alive_]*10))*
    ((population_[,"DIAB_DUR"][alive_])^parameter_[,"PVD_Shape"])
  
  PVD_end <- exp(parameter_[,"PVD_Cons"]+
                    parameter_[,"PVD_AgeDiag"]*
                   (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                    parameter_[,"PVD_smoker"]*population_[,"SMO"][alive_]+
                    parameter_[,"PVD_SBP_div10"]*
                   (population_[,"SBP"][alive_]/10)+
                    parameter_[,"PVD_HbA1c"]*population_[,"HBA"][alive_]+
                    parameter_[,"PVD_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"PVD_LDL_mult10"]*
                   (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"PVD_Shape"])
  
  prob_PVD <- 1 - exp(PVD_start - PVD_end)
  
  #Make PVD impossible for people with PVD history
  prob_PVD <- ifelse(population_[,"PVD_H"]==1, 0, prob_PVD)
  #remove derived variables that are not returned
  rm(PVD_start, PVD_end)
  #Return the probability that someone has PVD
  return(prob_PVD)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_ATFIB is the matrix giving the probability of having ATFIB
##' next year from UKPDS 90

UKPDS_90_ATFIB        <- function(population_, parameter_, alive_){
  
  ATFIB_start <- exp(parameter_[,"ATFIB_CONS"]+
                       parameter_[,"ATFIB_AGEDAIG"]*
                       (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                       parameter_[,"ATFIB_BMI"]*population_[,"BMI"][alive_]
  )*(population_[,"DIAB_DUR"][alive_])
  
  ATFIB_end <- exp(parameter_[,"ATFIB_CONS"]+
                     parameter_[,"ATFIB_AGEDAIG"]*
                   (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                     parameter_[,"ATFIB_BMI"]*population_[,"BMI"][alive_]
  )*(population_[,"DIAB_DUR"][alive_]+1)
  
  prob_ATFIB <- 1 - exp(ATFIB_start - ATFIB_end)
  
  #Make ATFIB impossible for people with PVD history
  prob_ATFIB <- ifelse(population_[,"ATFIB_H"][alive_]==1, 0, prob_ATFIB)
  
  #remove unnecessary derived variables
  rm(ATFIB_start,ATFIB_end )
  return(prob_ATFIB)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_eGRFu60 is the vector giving the probability of having an eGFR
##'under 60 next year


UKPDS_90_binrary_peGFRu60 <- function(population_, parameter_, alive_){
  
  eGFR_start <- exp(parameter_[,"eGFR_bu60_CONS"]+
                      parameter_[,"eGFR_bu60_FEMALE"]*population_[,"FEMALE"][alive_]+
                      parameter_[,"eGFR_bu60_AFRO"]*population_[,"AFRO"][alive_]+
                      parameter_[,"eGFR_bu60_INDIAN"]*population_[,"INDIAN"][alive_]+
                      parameter_[,"eGFR_bu60_AGEDAIG"]*
                     (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                      parameter_[,"eGFR_bu60_SBP_div10"]*
                     (population_[,"SBP"][alive_]/10)+
                      parameter_[,"eGFR_bu60_BMI"]*population_[,"BMI"][alive_]+
                      parameter_[,"eGFR_bu60_HDL_div10"]*
                      (population_[,"HDL"][alive_]*10)+
                      parameter_[,"eGFR_bu60_LDL_div10"]*
                      (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_])^parameter_[,"eGFR_bu60_Shape"])
  
  eGFR_end <- exp(parameter_[,"eGFR_bu60_CONS"]+
                    parameter_[,"eGFR_bu60_FEMALE"]*population_[,"FEMALE"][alive_]+
                    parameter_[,"eGFR_bu60_AFRO"]*population_[,"AFRO"][alive_]+
                    parameter_[,"eGFR_bu60_INDIAN"]*population_[,"INDIAN"][alive_]+
                    parameter_[,"eGFR_bu60_AGEDAIG"]*
                    (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                    parameter_[,"eGFR_bu60_SBP_div10"]*
                    (population_[,"SBP"][alive_]/10)+
                    parameter_[,"eGFR_bu60_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"eGFR_bu60_HDL_div10"]*
                    (population_[,"HDL"][alive_]*10)+
                    parameter_[,"eGFR_bu60_LDL_div10"]*
                    (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"eGFR_bu60_Shape"])
  
  prob_eGRFu60 <- 1 - exp(eGFR_start - eGFR_end)
  
  #remove unecessary derived variables
  rm(eGFR_start,eGFR_end)
  
  #People with eGFR < 60 cannot go above 60 in the UKPDS 90 framework 
  #(it is a time to event equation)
  #For these people, the probability that eGFR is under 60 is set to 1
  prob_eGRFu60 <- ifelse(population_[,"eGFR"][alive_] < 60, 1, prob_eGRFu60)
  
  return(prob_eGRFu60)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@param eGRFu60_, is a vector, when it is equal to 1 the person has an eGFR
##'of under 60, 0 otherwise
##'@return egfr is the eGFR of the each patient next year

UKPDS_90_eGFR         <- function(population_, parameter_, eGRFu60_, alive_){
  FV_u60 <- parameter_[,"eGFR_cu60_CONS"]+
    parameter_[,"eGFR_cu60_Female"]*population_[,"FEMALE"][alive_]+
    parameter_[,"eGFR_cu60_AFRO"]*population_[,"AFRO"][alive_]+
    parameter_[,"eGFR_cu60_INDIAN"]*population_[,"INDIAN"][alive_]+
    parameter_[,"eGFR_cu60_eGFR"]*population_[,"eGFR"][alive_]+
    parameter_[,"eGFR_cu60_firsteGFR"]*population_[,"eGFR_0"][alive_]+ 
    parameter_[,"eGFR_cu60_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1)
  
    pnorm1 <- pnorm((0-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    pnorm2 <- pnorm((60-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    dnorm1 <- dnorm((0-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    dnorm2 <- dnorm((60-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    
  FV_o60 <- parameter_[,"eGFR_co60_CONS"]+
    parameter_[,"eGFR_co60_Female"]*population_[,"FEMALE"][alive_]+
    parameter_[,"eGFR_co60_AFRO"]*population_[,"AFRO"][alive_]+
    parameter_[,"eGFR_co60_INDIAN"]*population_[,"INDIAN"][alive_]+
    parameter_[,"eGFR_co60_eGFR"]*population_[,"eGFR"][alive_]+
    parameter_[,"eGFR_co60_firsteGFR"]*population_[,"eGFR_0"][alive_]+ 
    parameter_[,"eGFR_co60_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1) 
    
    pnorm3 <- pnorm((60-FV_o60)/parameter_[,"eGFR_co60_Shape"])
    dnorm3 <- dnorm((60-FV_o60)/parameter_[,"eGFR_co60_Shape"])
    
    #If the patient's egfr is predicted to be under 60 apply one tobit model, if 
    #not apply the other
    egfr <-ifelse(eGRFu60_==1,
                 FV_u60-parameter_[,"eGFR_cu60_Shape"]*((dnorm1-dnorm2)/(pnorm1-pnorm2)),
                 FV_o60+parameter_[,"eGFR_co60_Shape"]*((dnorm3)/(1-pnorm3)))
    
    ##remove other all dervied values except current eGFR
    rm(FV_u60, FV_o60, pnorm1, pnorm2, dnorm1, dnorm2, pnorm3, dnorm3)
    
    return(egfr)
}

UKPDS_90_underlying_traj <- function(population_, coef_, endtime_){
  #store all results
  HbA1c_UKPDStraj   <- UKPDS_90_contrisk_A1c(population_, coef_, endtime_)
  SBP_UKPDStraj     <- UKPDS_90_contrisk_SBP(population_, coef_, endtime_)
  LDL_UKPDStraj     <- UKPDS_90_contrisk_LDL(population_, coef_, endtime_)
  HDL_UKPDStraj     <- UKPDS_90_contrisk_HDL(population_, coef_, endtime_)
  BMI_UKPDStraj     <- UKPDS_90_contrisk_BMI(population_, coef_, endtime_)
  HR_UKPDStraj      <- UKPDS_90_HEARTR(population_, coef_, endtime_)
  WBC_UKPDStraj     <- UKPDS_90_WBC(population_, coef_, endtime_)
  HAEM_UKPDStraj    <- UKPDS_90_HAEM(population_, coef_, endtime_)
  #push these matrices to the global environment
  for (variable in ls()) {
    assign(variable, get(variable), envir = .GlobalEnv)
  }
 
}