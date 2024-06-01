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
row <- 1 #1=deterministic
A1c <- initialise_intervention_dt_HbA1c(1,
                                        "MLexample",
                                        data[row,],
                                        20,
                                        GlobalVars)


##count the treatment effects and make sure that there are as many as the treatment effect duration
number_TEs <- sum(ifelse((A1c %in% data[row,"ML_Example_HbA1c"])==T,1,0))
if(number_TEs != data[row,"ML_Example_EffectDuration"]){
  stop("the modelled treatment effect duration does not match the number of years the treatment effect is applied for in the model")
}
rm(A1c)
}
