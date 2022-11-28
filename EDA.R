rm(list=ls())


# Set working directory ---------------------------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Read full data ---------------------------

full_dt <- read_sav("../Coronavirus Pandemic_Merge_AllData.SAV") #should have all the network components, "wide dataset"
full_dt <- as.data.frame(full_dt)
glimpse(full_dt)
str(full_dt)


# Correct for missing data ---------------------------

## Read data
use_to_select_dt <- read_sav("../20210519_merge_USETOSELECT_N=1101.sav")
View(use_to_select_dt)

## Filter for IDs used to select
dt <- 
full_dt %>% 
  filter(StartDate %in% use_to_select_dt$startdate) %>%
  filter(REMOVE == 0)

dim(dt)
#View(dt)
n <- nrow(dt)


# Sample Descriptives ---------------------------

## age
summary(dt$DEMO2); sd(dt$DEMO2, na.rm = TRUE) #age
table(dt$Screen1, exclude = NULL) # see CADRE CLC Data Project5/Qualtrics Codebooks/Coronavirus_Pandemic_A_Community_Survey.docx
age <- dt$DEMO2

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

gender_3cat <- recode_factor(gender, 
                             .default = "non-binary/other/prefer not to answer",
                             "2" = "cisgender_female",
                             "1" = "cisgender_male"
)
table(gender_3cat, exclude = NULL)                             
class(gender_3cat)
gender_3cat <- relevel(gender_3cat, ref = "cisgender_female")

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

table(dt$DEMO4SUM, exclude = NULL) #validity check - not relevant for analysis

race_4cat <- rep(NA, n)
race_4cat[which(dt$DEMO4_1 == 1)] <- "White"
race_4cat[which(dt$DEMO4_2 == 1)] <- "Black"

race <- cbind.data.frame(dt$DEMO4_1, dt$DEMO4_2, dt$DEMO4_3, dt$DEMO4_4, dt$DEMO4_5, 
                         dt$DEMO4_6, dt$DEMO4_7, dt$DEMO4_8, dt$DEMO4_9, dt$DEMO4_10,
                         dt$DEMO4_11, dt$DEMO4_12, dt$DEMO4_13, dt$DEMO4_14)


race_row_sum <- rowSums(race, na.rm = TRUE)
multi_race <- which(race_row_sum > 1)
race_4cat[multi_race] <- "Other/Multiple Race"

asian <- c(
  which(dt$DEMO4_4 == 1),
  which(dt$DEMO4_5 == 1),
  which(dt$DEMO4_6 == 1),
  which(dt$DEMO4_7 == 1),
  which(dt$DEMO4_8 == 1),
  which(dt$DEMO4_9 == 1),
  which(dt$DEMO4_10 == 1)
)
asian <- sort(unique(asian))
race_4cat[asian] <- "Asian"


(table(race_4cat, exclude = NULL))

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
ethnicity <- recode_factor(ethnicity, 
                    "0" = "non-hispanic",
                    "1" = "hispanic")
ethnicity <- relevel(ethnicity, ref = "non-hispanic")

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

## education
table(dt$DEMO12, exclude = NULL)
dt$DEMO12 <- haven::as_factor(dt$DEMO12)
class(dt$DEMO12)

education <- 
  dt %>%
  pull(DEMO12) %>%
  recode_factor("8th grade or below" = "High School or Less",
                "Some High School" = "High School or Less",
                "High School Graduate/Equivalent" = "High School or Less" 
         )
table(education, exclude=NULL)

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

essential_worker <- recode_factor(essential_worker,
                                  "0" = "no",
                                  "1" = "yes")
essential_worker <- relevel(essential_worker,
                            ref = "no")

## dwelling ownership
dt$DEMO6 <- haven::as_factor(dt$DEMO6)
dwelling_ownership <- 
  dt %>%
  pull(DEMO6) %>%
  recode("Own a home or condo" = "1",
         .default = "0",
  )
table(dwelling_ownership, exclude=NULL)

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

table(dt$NDAYSDRK, exclude = NULL)
daily_drinking <-
  dt %>% 
  pull(NDAYSDRK) %>% 
  recode_factor("0" = "none", "7" = "daily", .default = "1-6 days")
table(daily_drinking)

## opioids
opi_cats_all <- cbind(dt$USED7OPI_1, dt$USED7OPI_2, dt$USED7OPI_3,
                      dt$USED7OPI_4, dt$USED7OPI_5, dt$USED7OPI_6,
                      dt$USED7OPI_7)

opi_cats_colsums <- colSums(opi_cats_all, na.rm = T)
sum(opi_cats_colsums)
sum(opi_cats_colsums)/nrow(dt)

table(dt$NDAYSANYOPI)

daily_opioid <-
  dt %>% 
  pull(NDAYSANYOPI) %>% 
  recode_factor("0" = "none", "7" = "daily", .default = "1-6 days")
table(daily_opioid)

## stimulants
table(dt$USED7STIM, exclude = NULL)
table(dt$USED7STIM, exclude = NULL)/sum(table(dt$USED7STIM, exclude = NULL))
table(dt$NDAYSSTIM, exclude = NULL)

daily_stimulant <-
  dt %>% 
  pull(NDAYSSTIM) %>% 
  recode_factor("0" = "none", "7" = "daily", .default = "1-6 days")
table(daily_stimulant)

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


nasal_neg <- which(dt$LSQ12a1 == 2) #negative tests
blood_neg <- which(dt$LSQ12b1 == 2)

length(nasal_neg)
length(blood_neg)
identical(nasal_neg, blood_neg)

neg_covid_test <- unique(c(nasal_neg,  blood_neg))
length(neg_covid_test)


nasal_pos <- which(dt$LSQ12a1 == 1) #positive tests
blood_pos <- which(dt$LSQ12b1 == 1)

length(nasal_pos)
length(blood_pos)
identical(nasal_pos, blood_pos)

pos_covid_test <- unique(c(nasal_pos,  blood_pos))
length(pos_covid_test)


covid_test <- rep("no_test_or_unsure", n)
covid_test[neg_covid_test] <- "negative"
covid_test[pos_covid_test] <- "positive"
covid_test <- as.factor(covid_test)

table(covid_test, exclude = NULL) #dichotomize
covid_test <- relevel(covid_test, ref = "no_test_or_unsure")




# Collect all covariates for Tables 3-4 ---------------------------

cov_dt <- cbind.data.frame(
  race_4cat,
  household_size = dt$RDEMO8,
  dwelling_ownership,
  age,
  gender_3cat,
  ethnicity,
  essential_worker,
  income, 
  education,
  covid_test,
  daily_drinking,
  daily_opioid,
  daily_stimulant
)
dim(cov_dt)

cov_dt_na.omit <- na.omit(cov_dt)
dim(cov_dt_na.omit)

dt <- cbind.data.frame(dt, cov_dt)
dim(dt)

# Compute Primary Outcome Variable statistics (for Table 3) ---------------------------

summary(dt$CDCGAQ)

## any missing values in the 13 criteria
cdc <- cbind(dt$CDC1, dt$CDC2, dt$CDC3, dt$CDC4,
             dt$CDC5, dt$CDC6, dt$CDC7, dt$CDC8,
             dt$CDC9, dt$CDC10, dt$CDC11, dt$CDC12,
             dt$CDC13)
dim(cdc)
which(is.na(cdc))

cdc_avg_out <- apply(cdc, 1, mean)

head(dt$CDCGAQ)
head(cdc_avg_out)

sum.to.avg.ratio <- dt$CDCGAQ/cdc_avg_out 

summary(sum.to.avg.ratio)
na.val <- which(is.na(sum.to.avg.ratio)) #na is because one person reported 0s on all criteria

dt$CDCGAQ[na.val]
cdc_avg_out[na.val]


# Compute Secondary Outcome Variable statistics (for Table 4) ---------------------------

dt$LSQ12 <- haven::as_factor(dt$LSQ12)
table(dt$LSQ12, exclude = NULL)
tested_for_covid <- recode(dt$LSQ12, 
                                  "Yes" = 1,
                                  "No" = 0,
                                  "Unsure" = 0) 
table(tested_for_covid, exclude = NULL)


# Compute Secondary Outcome Variable statistics and Covariates (for Table 5) ---------------------------
## (accounting for covariates, in the subset of participants
## reporting a COVID-19 test (n=279).) 

## dataset
tested_for_covid_dt <- 
  dt %>% 
  filter(tested_for_covid == 1)

dim(tested_for_covid_dt)

## covariates
cov_tested_for_covid_dt <- 
 tested_for_covid_dt %>%
  select( 
    race_4cat,
    household_size,
    dwelling_ownership,
    age,
    gender_3cat,
    ethnicity,
    essential_worker,
    income, 
    education,
    covid_test,
    daily_drinking,
    daily_opioid,
    daily_stimulant
    )
  
dim(cov_tested_for_covid_dt)

## outcome
table(cov_tested_for_covid_dt$covid_test, exclude = NULL)

tab5_outcome <- 
  recode(cov_tested_for_covid_dt$covid_test,
        "positive" = 1,
        "negative" = 0,
        "no_test_or_unsure" = 0
        )
class(tab5_outcome)
table(tab5_outcome)


# Table 2: Final Data (to be compared to Monnig 2021)---------------------------

summary(age); sd(age)
summary(gender_3cat)
table(race_4cat, exclude = NULL); table(race_4cat, exclude = NULL)/sum(table(race_4cat, exclude = NULL)) 
table(ethnicity, exclude=NULL); table(ethnicity, exclude=NULL)/sum(table(ethnicity, exclude=NULL))
table(education, exclude = NULL); table(education, exclude = NULL)/sum(table(education, exclude = NULL))
table(income)
summary(dt$RDEMO8); sd(dt$RDEMO8, na.rm = TRUE) #household size
table(dwelling_ownership, exclude=NULL); table(dwelling_ownership, exclude=NULL)/sum(table(dwelling_ownership, exclude=NULL))
table(essential_worker, exclude = NULL); table(essential_worker, exclude = NULL)/sum(table(essential_worker, exclude = NULL))
summary(cdc_avg_out)
table(covid_test, exclude=NULL); table(covid_test, exclude=NULL)/sum(table(covid_test, exclude=NULL))
length(pos_covid_test)
table(dt$USED7SUB_1, exclude = NULL)/sum(table(dt$USED7SUB_1, exclude = NULL)) #cigarette use 
  ## DAILY SMOKING CATEGORIZATION NOT CLEAR
table(daily_drinking)/sum(table(daily_drinking))
table(daily_opioid); table(daily_opioid)/sum(table(daily_opioid))
table(daily_stimulant); table(daily_stimulant)/sum(table(daily_stimulant))


# Save image ---------------------------

save.image(file="eda.RData")

