rm(list=ls())


# Load libraries ---------------------------

library(haven)
library(dplyr)

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")
dt <- read_sav("../Coronavirus Pandemic_Merge_AllData.SAV") #should have all the network components, "wide dataset"
dt <- as.data.frame(dt)
glimpse(dt)
str(dt)

net <- read_sav("../Social Networks_ Substance Use and COVID Prevention.sav")
net <- as.data.frame(net)
dim(net)
sort(colnames(net))

class(dt$FUVH_MEAN) # main outcome
summary(dt$FUVH_MEAN)
table(dt$FUVH_MEAN, exclude = NULL) #vaccine hesitancy for alters? 
                                    #for egos, only for unvaccinated persons 
                                    #(for alters, only vaccination status is measured
                                    # we don't know about vaccine hesitancy scores for the alters per se)

# Sample Descriptives ---------------------------

## age
summary(dt$DEMO2); sd(dt$DEMO2, na.rm = TRUE) #age
table(dt$Screen1) # see CADRE CLC Data Project5/Qualtrics Codebooks/Coronavirus_Pandemic_A_Community_Survey.docx

## sex and gender identity
table(dt$Screen2) #sex assigned at birth, 1=male, 2=female
table(dt$Screen2, exclude = NULL)/sum(table(dt$Screen2))

## race 
  # DEMO4 What is your race? [Choose all that apply]:
  # ▢	White  (1) 
  # ▢	Black or African American  (2) 
  # ▢	American Indian or Alaska Native  (3) 
  # ▢	Asian Indian  (4) 
  # ▢	Chinese  (5) 
  # ▢	Filipino  (6) 
  # ▢	Japanese  (7) 
  # ▢	Korean  (8) 
  # ▢	Vietnamese  (9) 
  # ▢	Other Asian  (10) 
  # ▢	Native Hawaiian  (11) 
  # ▢	Guamanian or Chamorro  (12) 
  # ▢	Samoan  (13) 
  # ▢	Other Pacific Islander  (14) 
  # ▢	Don't Know  (15) 
table(dt$DEMO4_1, exclude = NULL)
table(dt$DEMO4_2, exclude = NULL)
table(dt$DEMO4_3, exclude = NULL) 

# QTS: 
 #- these numbers are different from the published estimates
 #- is there an aggregate variable?
 # see below for "DEMO4SUM"?

table(dt$DEMO4SUM) #code interpretation is not clear to me

# screening variable below:
table(dt$Screen3, exclude = NULL)/(sum(table(dt$Screen3, exclude = NULL)))
# Screening Code: White  (1), Black/African American  (3) 
# Asian  (5), American Indian/Alaska Native  (7) 
# Native Hawaiian/Pacific Islander  (9), Two or more races  (11) 
# Other race  (13) 

## ethnicity

# DEMO3 Are you Hispanic, Latino/Latina, or of Spanish origin? [Choose all that apply]:
  # ▢	No, not of Hispanic, Latino/Latina, or Spanish Origin  (1) 
  # ▢	Yes, Mexican, Mexican American, or Chicano/Chicana  (2) 
  # ▢	Yes, Puerto Rican  (3) 
  # ▢	Yes, Cuban  (4) 
  # ▢	Yes, Another Hispanic, Latino/Latina, or Spanish origin  (5) 
  # ▢	Don't Know  (6) 
table(dt$DEMO3_1, exclude = NULL)
table(dt$DEMO3_2, exclude = NULL)
table(dt$DEMO3_3, exclude = NULL) #SEE ABOVE QTS FOR RACE 

table(dt$Screen4, exclude = NULL)
table(dt$Screen4, exclude = NULL)/(sum(table(dt$Screen4, exclude = NULL)))


# Table 1  (Monnig et al, 2021) ---------------------------
