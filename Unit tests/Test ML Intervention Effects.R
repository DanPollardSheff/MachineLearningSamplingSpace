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

##run the HbA1c intervention function on some example PSAs
##Should be the same for all people in the model, so set to 1 person
##Use the machine learning example
##Use the first row of data
##run for 20 years

##Loop over all PSA parameters and the deterministic model run
for(row in 1:length(data[,"ML_Example_HbA1c"])){
A1c <- initialise_intervention_dt_HbA1c(1,
                                        "MLexample",
                                        data[row,],
                                        max(data[,"ML_Example_EffectDuration"]),
                                        GlobalVars)


##count the treatment effects and make sure that there are as many as the treatment effect duration
number_TEs <- sum(ifelse((A1c %in% data[row,"ML_Example_HbA1c"])==T,1,0))
if(number_TEs != data[row,"ML_Example_EffectDuration"]){
  stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
}
rm(A1c,number_TEs)
}

##BMI
##Loop over all PSA parameters and the deterministic model run
for(row in 1:length(data[,"ML_Example_BMI"])){
  BMI <- initialise_intervention_dt_BMI(1,
                                          "MLexample",
                                          data[row,],
                                          max(data[,"ML_Example_EffectDuration"]),
                                          GlobalVars)
  
  
  ##count the treatment effects and make sure that there are as many as the treatment effect duration
  number_TEs <- sum(ifelse((BMI %in% data[row,"ML_Example_BMI"])==T,1,0))
  if(number_TEs != data[row,"ML_Example_EffectDuration"]){
    stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
  }
  rm(BMI,number_TEs)
}

##SBP
for(row in 1:length(data[,"ML_Example_SBP"])){
  SBP <- initialise_intervention_dt_SBP(1,
                                        "MLexample",
                                        data[row,],
                                        max(data[,"ML_Example_EffectDuration"]),
                                        GlobalVars)
  
  
  ##count the treatment effects and make sure that there are as many as the treatment effect duration
  number_TEs <- sum(ifelse((SBP %in% data[row,"ML_Example_SBP"])==T,1,0))
  if(number_TEs != data[row,"ML_Example_EffectDuration"]){
    stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
  }
  rm(SBP,number_TEs)
}

##HDL 
for(row in 1:length(data[,"ML_Example_SBP"])){
  HDL <- initialise_intervention_dt_HDL(1,
                                        "MLexample",
                                        data[row,],
                                        max(data[,"ML_Example_EffectDuration"]),
                                        GlobalVars)
  
  
  ##count the treatment effects and make sure that there are as many as the treatment effect duration
  number_TEs <- sum(HDL[2:length(HDL)]) #we don't want the first column as this is an ID column
  
  if(number_TEs != 0){
    stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
  }
  rm(HDL,number_TEs)
}

##LDL
for(row in 1:length(data[,"ML_Example_SBP"])){
  LDL <- initialise_intervention_dt_LDL(1,
                                        "MLexample",
                                        data[row,],
                                        max(data[,"ML_Example_EffectDuration"]),
                                        GlobalVars)
  
  
  ##count the treatment effects and make sure that there are as many as the treatment effect duration
  number_TEs <- sum(ifelse((LDL %in% data[row,"ML_Example_LDL"])==T,1,0))
  if(number_TEs != data[row,"ML_Example_EffectDuration"]){
    stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
  }
  rm(LDL,number_TEs)
}