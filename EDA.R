rm(list=ls())


# Set working directory ---------------------------

data_loc <- ("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT/")


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Read full data ---------------------------

full_dt <- read_sav(paste0(data_loc, 
                           "../Coronavirus Pandemic_Merge_AllData.SAV")) #should have all the network components, "wide dataset"
full_dt <- as.data.frame(full_dt)
glimpse(full_dt)
str(full_dt)


# Correct for missing data ---------------------------

## Read data
use_to_select_dt <- read_sav(
  paste0(data_loc,
  "../20210519_merge_USETOSELECT_N=1101.sav"))
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
cdc <- as.data.frame(
  cbind(CDC1=dt$CDC1, CDC2=dt$CDC2, CDC3=dt$CDC3, CDC4=dt$CDC4,
        CDC5=dt$CDC5, CDC6=dt$CDC6, CDC7=dt$CDC7, CDC8=dt$CDC8,
        CDC9=dt$CDC9, CDC10=dt$CDC10, CDC11=dt$CDC11, CDC12=dt$CDC12,
        CDC13=dt$CDC13))
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

dt$cdc_avg_out <- cdc_avg_out


cdc_scores <- cbind.data.frame(cdc, cdc_avg_out, MTURKID=dt$MTURK1)


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

# Create dataset with Table 2 variables and MTURKD IDs ---------------------------
# to be used in comparing the characteristics and behaviors among 
# people providing FUSN consent vs not

tab2_dt <- cbind(
  MTURK1=dt$MTURK1,
  FUSNCONSENT=dt$FUSNCONSENT,
  age, 
  gender_3cat,
  race_4cat,  
  ethnicity,  
  education,
  income,
  household_size=dt$RDEMO8, 
  dwelling_ownership, 
  essential_worker, 
  as.numeric(cdc_avg_out),
  covid_test, 
  #pos_covid_test,
  sub_last7days=dt$USED7SUB_1,
  daily_drinking,
  daily_opioid, 
  daily_stimulant
  )


# Vaccine information---------------------------

## at least one does of the vaccine
head(dt$FUVA3)
table(dt$FUVA3, exclude = NULL)


# Vaccine hesitancy ---------------------------

# VH1 Would you take a COVID-19 vaccine if offered?
#   o	Definitely  (1) 
#   o	Probably  (2) 
#   o	I may or I may not  (3) 
#   o	Probably not  (4) 
#   o	Definitely not  (5) 
#   o	Don't know  (6) 

# VH2 When a COVID-19 vaccine is available:
#   o	I will want to get it as soon as possible  (1) 
#   o	I will take it when offered  (2) 
#   o	I'm not sure what I will do  (3) 
#   o	I will put off (delay) getting it  (4) 
#   o	I will refuse to get it  (5) 
#   o	Don't know  (6) 

# VH3 I would describe my attitude towards receiving a COVID-19 vaccine as:
#   o	Very positive  (1) 
# o	Pretty positive  (2) 
# o	Neutral  (3) 
# o	Pretty Negative  (4) 
# o	Very Negative  (5) 
# o	Don't know  (6) 

# VH4 If a COVID-19 vaccine was available at my pharmacy, I would:
#   o	Get it as soon as possible  (1) 
# o	Get it when I have time  (2) 
# o	Delay getting it  (3) 
# o	Avoid getting it for as long as possible  (4) 
# o	Never get it  (5) 
# o	Don't know  (6) 

# VH5 If my family or friends were thinking of getting a COVID-19 vaccination, I would:
#   o	Strongly encourage them  (1) 
# o	Encourage them  (2) 
# o	Not say anything about it  (3) 
# o	Ask them to delay getting the vaccination  (4) 
# o	Suggest they do not get the vaccination  (5) 
# o	Don't know  (6) 


# VH6 I would describe myself as:
#   o	Eager to get a COVID-19 vaccine  (1) 
# o	Willing to get the COVID-19 vaccine  (2) 
# o	Not bothered about getting the COVID-19 vaccine  (3) 
# o	Unwilling to get the COVID-19 vaccine  (4) 
# o	Anti-vaccination for COVID-19  (5) 
# o	Don't know  (6) 

# VH7 Taking a COVID-19 vaccination is:
#   o	Really important  (1) 
# o	Important  (2) 
# o	Neither important nor unimportant  (3) 
# o	Unimportant  (4) 
# o	Really unimportant  (5) 
# o	Don't know  (6) 

# # VH10 How confident are you that the coronavirus vaccines that are available have been tested for safety and efficacy?
#   o	Very confident  (1) 
# o	Somewhat confident  (2) 
# o	Not too confident  (3) 
# o	Not at all confident  (4) 
# o	Don't know  (5) 

# VH11 How confident are you that coronavirus vaccines are being distributed in a way that is fair?
#   o	Very confident  (1) 
# o	Somewhat confident  (2) 
# o	Not too confident  (3) 
# o	Not at all confident  (4) 
# o	Don't know  (5) 


v_hesitancy_info <- 
  as.data.frame(
    cbind(vh1=dt$FUVH1, vh2=dt$FUVH2, vh3=dt$FUVH4, vh4=dt$FUVH4,
        vh5=dt$FUVH5, vh6=dt$FUVH6, vh7=dt$FUVH7, 
        #vh9=dt$FUVH,
        vh10=dt$FUVH10, vh11=dt$FUVH11)
    )
dim(v_hesitancy_info)
head(v_hesitancy_info)
which(is.na(v_hesitancy_info))

vh_info_avg_out <- apply(v_hesitancy_info, 1, mean)

vh_info_scores <- cbind.data.frame(v_hesitancy_info, vh_info_avg_out, MTURKID=dt$MTURK1)


# Vaccine access---------------------------

# VA1 Where do you get your news or information about the CORONAVIRUS VACCINE? Please check all that apply.
# ▢	Television news  (1) 
# ▢	Radio broadcast  (2) 
# ▢	Print or online newspaper  (3) 
# ▢	Social media  (4) 
# ▢	State or local government (including press briefings or official websites)  (5) 
# ▢	Centers for Disease Control and Prevention  (6) 
# ▢	White House Press Briefing  (7) 
# ▢	Other (please specify):  (8) ________________________________________________
# ▢	None of the above  (9) 

 
# VA2 What social media apps do you use?
#   ▢	Facebook  (1) 
# ▢	Youtube  (2) 
# ▢	Twitter  (3) 
# ▢	WhatsApp  (4) 
# ▢	Instagram  (5) 
# ▢	Parler  (6) 
# ▢	Gab  (7) 
# ▢	TikTok  (8) 
# ▢	WeChat  (9) 
# ▢	Other:  (10) ________________________________________________
# ▢	None  (11) 
# 

# VA3 Have you gotten at least one dose of the coronavirus vaccine?
#   o	Yes  (1) 
# o	No  (2) 
# o	Unsure  (3) 

# Display This Question:
#   If Have you gotten at least one dose of the coronavirus vaccine? = No
# 
# 
# VA4 Have you been offered the vaccine?
#   o	Yes  (1) 
# o	No  (2) 
# o	Unsure  (3) 
# 

# 
# VA5 Was the vaccine offered through your workplace?
#   o	Yes  (1) 
# o	No  (2) 
# o	Unsure  (3) 
# 
# 
# Page Break	
# 
# Display This Question:
#   If Have you been offered the vaccine? = No
# 
# 
# VA6 If you have not been offered the vaccine, are you eligible based on state guidelines?
#   o	Yes  (1) 
# o	No  (2) 
# o	Not sure  (3) 
# 

# 
# Display This Question:
#   If If you have not been offered the vaccine, are you eligible based on state guidelines? = Yes
# 
# 
# VA7 If you are eligible and have not been able to get the vaccine, what are the reasons why not? Please select all that apply:
#   ▢	Did not have transportation  (1) 
# ▢	Unable to obtain an appointment  (2) 
# ▢	Did not have childcare  (3) 
# ▢	Schedule conflicts or did not have time  (5) 
# ▢	Unwilling or hesitant to get the vaccine  (6) 
# ▢	Other reason  (7) 
# 

# 
# Display This Question:
#   If Have you gotten at least one dose of the coronavirus vaccine? = Yes
# 
# 
# VA8 Which month did you get the COVID-19 vaccine? If two does were required, which month did you get your first dose?
#   o	Before December 2020 - I participated in a clinical trial  (1) 
# o	December 2021  (2) 
# o	January 2021  (3) 
# o	February 2021  (4) 
# o	March 2021  (5) 
# o	April 2021  (6) 
# o	May 2021  (7) 
# 
#
# Display This Question:
#   If Have you gotten at least one dose of the coronavirus vaccine? = Yes
# 
# 
# VA9 Which brand of the COVID vaccine did you receive?
#   o	Moderna  (1) 
# o	Pfizer  (2) 
# o	Johnson & Johnson  (4) 
# o	Don't know  (5) 
# 
	
#  
# Display This Question:
# If Which brand of the COVID vaccine did you receive? = Moderna
# Or Which brand of the COVID vaccine did you receive? = Pfizer
# Or Which brand of the COVID vaccine did you receive? = Johnson & Johnson
#  
# 
# VA10 Was this the brand of vaccine that you preferred?
# o	Yes  (1) 
# o	No  (2) 
# o	Unsure or no preference  (3) 
# 
# 
# Page Break	
#  
# Display This Question:
# If Was this the brand of vaccine that you preferred? = No
#  
# 
# VA11 Which brand of vaccine would you rather have gotten?
# o	Moderna  (1) 
# o	Pfizer  (2) 
# o	Johnson & Johnson  (3) 
# o	Don't know  (4) 
# 
# 


v_access_info <- 
  as.data.frame(
    cbind(va3=dt$FUVA3, va4=dt$FUVA4)
  )

dim(v_access_info)
head(v_access_info)
which(is.na(v_hesitancy_info))
which(!is.na(v_hesitancy_info))

# va_info_avg_out <- apply(v_hesitancy_info, 1, mean)

vaccess_info_scores <- cbind.data.frame(v_access_info,
                                   MTURKID=dt$MTURK1)
head(vaccess_info_scores)


# Save RDS object ---------------------------

eda_env <- new.env()
eda_env$dt <- dt
eda_env$tab2_dt <- tab2_dt
eda_env$cdc_scores <- cdc_scores
eda_env$vh_info_scores <- vh_info_scores
eda_env$vaccess_info_scores <- vaccess_info_scores

saveRDS(eda_env, paste0(data_loc, "eda_objects.rds"))




