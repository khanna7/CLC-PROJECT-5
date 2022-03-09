rm(list=ls())


# Load libraries ---------------------------

library(haven)
library(dplyr)

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")
dt <- read_sav("../Coronavirus Pandemic_Merge_AllData.SAV") #should have all the network components, "wide dataset"
dt <- as.data.frame(dt)
glimpse(dt)
str(dt)
n <- nrow(dt)

missing_id_dt <- read_sav("../20210519_merge_USETOSELECT_N=1101.sav")
View(missing_id_dt)
  
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

table(dt$DEMO25_1, exclude = NULL) #gender identity = man
table(dt$DEMO25_2, exclude = NULL) #gender identity = woman
table(dt$DEMO25_3, exclude = NULL) #gender identity = non-binary
table(dt$DEMO25_4, exclude = NULL) #gender identity = transgender
table(dt$DEMO25_5, exclude = NULL) #gender identity = none of the above
table(dt$DEMO25_7, exclude = NULL) #gender identity = prefer not to answer

gender = rep(NA, n)

man <- which(dt$DEMO25_1 == 1)
woman <- which(dt$DEMO25_2 == 1)
non_binary <- which(dt$DEMO25_3 == 1)
trans <- which(dt$DEMO25_4 == 1)
no_gender <- which(dt$DEMO25_5 == 1)
missing_gender <- which(dt$DEMO25_7 == 1)

gender[man] <- 1
gender[woman] <- 2
gender[non_binary] <- 3
gender[trans] <- 4
gender[no_gender] <- 5
gender[missing_gender] <- 7

table(gender, exclude = NULL)


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

table(dt$DEMO4SUM, exclude = NULL) #code interpretation is not clear to me

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

ethnicity = rep(NA, n)
not_hispanic <- which(dt$DEMO3_1 == 1)
hispanic <- which(is.na(dt$DEMO3_1))
mexican <- which(dt$DEMO3_2 == 1)
puerto_rican <- which(dt$DEMO3_3 == 1)
cuban <- which(dt$DEMO3_4 == 1)
another_latinx <- which(dt$DEMO3_5 == 1)
dont_know_ethnicity <- which(dt$DEMO3_6 == 1)

ethnicity[not_hispanic] <- 0
ethnicity[hispanic] <- 1 #binary classification

table(ethnicity, exclude = NULL)

## income
  # o	Less than $25,000  (4) 
  # o	$25,000 - $34,999  (5) 
  # o	$35,000 - $49,999  (6) 
  # o	$50,000 - $74,999  (7) 
  # o	$75,000 - $99,999  (8) 
  # o	$100,000 - $149,999  (9) 
  # o	$150,000 - $199,999  (10) 
  # o	$200,000 and above  (11) 
table(dt$DEMO20, exclude=NULL)
table(dt$DEMO20, exclude=NULL)/nrow(dt)
income=dt$DEMO20

## essential worker
  # o	Yes  (1) 
  # o	No  (2) 
  # o	Not Sure  (3) 
table(dt$LSQ3, exclude = NULL)
table(dt$LSQ3, exclude = NULL)/n

essential_worker <- rep(NA, n)
yes_essential_worker <- which(dt$LSQ3 == 1)
no2_essential_worker <- which(dt$LSQ3 == 2)
no3_essential_worker <- which(dt$LSQ3 == 3)

essential_worker[yes_essential_worker] <- 1
essential_worker[no2_essential_worker] <- 0 #dichotomize
essential_worker[no3_essential_worker] <- 0

table(essential_worker, exclude = NULL)

## substance use in past 7 days
## QT FOR ALL: How did we find the daily vs non-daily categorization?

## cigarettes (1) 
table(dt$USED7SUB_1, exclude = NULL)
table(dt$USED7SUB_1, exclude = NULL)/sum(table(dt$USED7SUB_1, exclude = NULL))

## E-cigarettes (1) ## QT: How did we find the daily vs non-daily categorization?
table(dt$USED7SUB_6, exclude = NULL)
table(dt$USED7SUB_6, exclude = NULL)/sum(table(dt$USED7SUB_1, exclude = NULL))

## cannabis (QT: Is this #10 in the questionnaire, -alone vs w/tobacco?)
table(dt$USED7SUB_10, exclude = NULL)
table(dt$USED7SUB_10, exclude = NULL)/sum(table(dt$USED7SUB_1, exclude = NULL))

## alcohol
table(dt$USED7SUB_9, exclude = NULL)
table(dt$USED7SUB_9, exclude = NULL)/sum(table(dt$USED7SUB_1, exclude = NULL))

## opioids
opi_cats_all <- cbind(dt$USED7OPI_1, dt$USED7OPI_2, dt$USED7OPI_3,
                      dt$USED7OPI_4, dt$USED7OPI_5, dt$USED7OPI_6,
                      dt$USED7OPI_7)

opi_cats_colsums <- colSums(opi_cats_all, na.rm = T)
sum(opi_cats_colsums)
sum(opi_cats_colsums)/nrow(dt)

## stimulants
table(dt$USED7STIM, exclude = NULL)
table(dt$USED7STIM, exclude = NULL)/sum(table(dt$USED7STIM, exclude = NULL))


## covid19 testing history: 
## LSQ12 Have you been tested for the novel coronavirus, or COVID-19?
   # Yes  (1) 
   #No (2) 
   #Unsure (3) 

table(dt$LSQ12, exclude = NULL)
table(dt$LSQ12, exclude = NULL)/sum(table(dt$LSQ12, exclude = NULL))

## COVID test result
table(dt$LSQ12a1, exclude = NULL)
table(dt$LSQ12b1, exclude = NULL)

nasal_pos <- which(dt$LSQ12a1 == 1)
blood_pos <- which(dt$LSQ12b1 == 1)

length(nasal_pos)
length(blood_pos)
identical(nasal_pos, blood_pos)

pos_covid_test <- unique(c(nasal_pos,  blood_pos))
length(pos_covid_test)

covid_test <- rep(0, n)
covid_test[pos_covid_test] <- 1
table(covid_test, exclude = NULL) #dichotomize

# TAB 2: Characteristics of the participants ---------------------------
# See above

# Covariates for Tables 3-5 ---------------------------

cov_dt <- cbind.data.frame(
  gender,
  ethnicity,
  essential_worker,
  income,
  covid_test
)
dim(cov_dt)

cov_dt_na.omit <- na.omit(cov_dt)
dim(cov_dt_na.omit)

# Primary Outcome (Table 3) ---------------------------


# TAB 3: GLM, outcome: substance use, COVs: CDC guideline adherence ---------------------------



# TAB 4: Logistic regression of SU vs any COVID-19 testing ---------------------------


# TAB 5: LR, outcome stimulant use w/ +COVID-19 test, ---------------------------
## accounting for covariates, in the subset of participants
## reporting a COVID-19 test (n=279). 
