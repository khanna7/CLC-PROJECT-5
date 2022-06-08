rm(list=ls())


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Set working directory ---------------------------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


# Load complete (individual+network) data ---------------------------

load(file="eda.RData") #from "../Coronavirus Pandemic_Merge_AllData.SAV") 
dim(dt)


# SN20 Does ${lm://Field/1} smoke tobacco, like cigarettes or cigars?
#FUA4...FUA8 are the 5 network members

cig_net_dt <- 
  dt %>% select(comp_code_Text_Set,
                FUA4_SN20,
                FUA5_SN20,
                FUA6_SN20,
                FUA7_SN20,
                FUA8_SN20
  )

View(cig_net_dt)

table(dt$FUSNCONSENT, exclude = NULL)

# Above data shows MTURK participants recruited in the survey
# Many didn't take the survey, only about n=200 did.
# Is there an easy way to identify those that can be taken out?
table(dt$FUSNCONSENT, exclude = NULL)

# Once we do that, then:

  # - we can take the proportions of network members for each person 
      # reporting a behavior

  # fit a logistic regression model that regresses teh outcome on the behaviors 
        # + any relevant individual attributes
        # model is limited because we are assuming independence across observations.
  
   # fit a regressoin model that accounts for the dependency in obs. 
        # e.g.: spatial autocorrelation etc. 
        # or other techniques of repeated measures analysis.


table(dt$FUA4_SN20, exclude = NULL)
table(dt$FUA5_SN20, exclude = NULL)
table(dt$FUA6_SN20, exclude = NULL)
table(dt$FUA7_SN20, exclude = NULL)      
table(dt$FUA8_SN20, exclude = NULL)      

