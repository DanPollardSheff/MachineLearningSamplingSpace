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


##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_ is the Global Variables matrix, this allows the duration of treatment 
##' effect to be controlled
##'@param attend_se_ is a TRUE/FALSE vector that indicates whether a patient attends
##' a structured education course in each year 
##'@return INTE_A1c is a matrix that gives the reduction in A1c for each patient 
##'in each year compared to a patient on a "normal" trajectory

initialise_intervention_dt_HbA1c <- function(n_,
                                             treatment_, 
                                             parameter_,
                                             endtime_, 
                                             GlobalVars_) {
  INTE_A1c <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_A1c[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test" | treatment_ == "Mt_HOOD_2018_QOL_A1c"){
    INTE_A1c[,2:(endtime_+2)] <- -0.5
  }else if(treatment_ == "MLexample"){
    INTE_A1c[,2:(parameter_[,"ML_Example_EffectDuration"]+1)] <- parameter_[,"ML_Example_HbA1c"]
  }else{ #if no treatment option is selected, leave them at baseline values
    INTE_A1c[,2:(endtime_+2)] <- 0
  }
  
  return(INTE_A1c)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@return INTE_BMI is a matrix that gives the reduction in BMI for each patient 
##'in each year

initialise_intervention_dt_BMI <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_) {
  INTE_BMI <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_BMI[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test"|treatment_ == "Mt_HOOD_2018_QOL_BMI"){
    INTE_BMI[,2:(endtime_+2)] <- -1
  }else if(treatment_ == "MLexample"){
    INTE_BMI[,2:(parameter_[,"ML_Example_EffectDuration"]+1)] <- parameter_[,"ML_Example_BMI"]
  }
  
  
  return(INTE_BMI)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@return INTE_SBP is a matrix that gives the reduction in SBP(mmHg) for each patient 
##'in each year

initialise_intervention_dt_SBP <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_) {
  INTE_SBP <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_SBP[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test" | treatment_ == "Mt_HOOD_2018_QOL_SBP"){
    INTE_SBP[,2:(endtime_+2)] <- -10
  }else if(treatment_ == "MLexample"){
    INTE_SBP[,2:(parameter_[,"ML_Example_EffectDuration"]+1)] <- parameter_[,"ML_Example_SBP"]
  }else{
    #If no option is specified, set treatment effects to 0
    INTE_SBP[,2:(endtime_+2)] <-0 
  }
  
  return(INTE_SBP)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@return INTE_HDL is a matrix that gives the change in high density lipoprotein cholesterol 
##'for each patient in each year
initialise_intervention_dt_HDL <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_HDL <- matrix(data=0, nrow = n_, ncol =(endtime_+2))
  INTE_HDL[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  
  if(treatment_ == "Mt_HOOD_2018_QOL_ALL" | treatment_ == "Mt_HOOD_2018_QOL_HDL"){
    INTE_HDL[,2:(endtime_+2)] <- -0.5
  }else if(treatment_ == "MLexample"){
    INTE_HDL[,2:(parameter_[,"ML_Example_EffectDuration"]+1)] <- parameter_[,"ML_Example_HDL"]
  }
  
  return(INTE_HDL)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@return INTE_LDL is a matrix that gives the reduction in HDL cholesterol 
##'for each patient in each year

initialise_intervention_dt_LDL <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_LDL <- matrix(data=0, nrow = n_, ncol = (endtime_+2))
  INTE_LDL[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  
  if(treatment_ == "Mt_HOOD_2018_QOL_ALL" | treatment_ == "Mt_HOOD_2018_QOL_LDL"){
    INTE_LDL[,2:(endtime_+2)] <- -0.5
  }else if(treatment_ == "MLexample"){
    INTE_LDL[,2:(parameter_[,"ML_Example_EffectDuration"]+1)] <- parameter_[,"ML_Example_LDL"]
  }
  return(INTE_LDL)
}

##'@param Input_Prob_ is a numeric number between 0 and 1
##'@param HR_ is a hazard ratio 
##'@return output_prob is the new probability after applying the hazard ratio

Hazard_Ratio_Intervention <- function(Input_Prob_,
                                      HR_){

  input_rate <- -log(1-Input_Prob_)/1
  output_rate <- input_rate*HR_
  output_prob <- 1-exp(-output_rate*1)
  
  return(output_prob)
  
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@return attend_se is a vector of probabilities 

initialise_intervention_dt_attendse <- function(n_, treatment_, parameter_){
  attend_se <- matrix(data=0, nrow = n_, ncol = 3)
  attend_se[,1] <- seq(from = 1, to = n_, by = 1)
  if(treatment_ == "Control_MetaAnalysis_all"| treatment_ == "Baseline") {
    #probability of attending SE
    attend_se_1 <- ifelse(runif(n_) < parameter_[, "Cont_Embedding_prob_attendSE"],1,0)
    attend_se[,2] <- attend_se_1
    #only one period for control, so no year 2 start of SE
  }else if (treatment_ == "Embedding_MetaAnalysis_All"|
            treatment_ == "Embedding_TrialEffect_All"){
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE"]
    p_attend_se_y1 <- temp/(1+temp)
    
    attend_se_1 <- ifelse(runif(n_) < p_attend_se_y1,1,0)
    attend_se[,2] <- attend_se_1
    
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE_2yr"]
    p_attend_se_y2 <- temp/(1+temp)
    
    #normalise based on the probability of attending SE in year 1
    p_attend_se_y2_nosey1 <- ((p_attend_se_y2 - p_attend_se_y1) / (1-p_attend_se_y1))
    
    attend_se_2 <- ifelse(runif(n_) < p_attend_se_y2_nosey1,1,0)
    attend_se[,3] <- attend_se_2
    
    #Overwrite the year 2 values with no attendance if they attend SE in year 1
    attend_se[,3] <- ifelse(attend_se[,2]==1,0, attend_se[,3])
    
  }else if (treatment_ == "Embedding_MetaAnalysis_1yr"|
            treatment_ == "Embedding_TrialEffect_1yr"){
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE"]
    p_attend_se_y1 <- temp/(1+temp)
    
    attend_se_1 <- ifelse(runif(n_) < p_attend_se_y1,1,0)
    attend_se[,2] <- attend_se_1
    
  }
  return(attend_se)
}
