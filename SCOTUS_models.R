##########################################################
## Program: SCOTUS_models.R                             ##
## Created by: Robert J. Reynolds, MPH PhD              ##
## Description: This program fits a Poisson model to    ##
##   person-time interval data.                         ##
## Date created: 15 February 2016                       ##
##########################################################

## Define the "Active" variable as a binary
  scotus3$active <- (scotus3$retiree-1)*(-1)


## Final published model
  summary(m1 <- glm(delta ~ age + year + ChiefJustice +
                            careerlength + active + offset(log(st)), 
                    family="poisson", data=scotus3))

## Get the MRRs and 95% CIs from the model output
  cbind(exp(m1$coefficients),exp(confint(m1,level=0.95)))[-1,]