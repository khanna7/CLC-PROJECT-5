# REPRODUCE INFERENTIAL TABLES (3-5) FROM PRIOR PAPER

rm(list=ls())


# Set working directory ---------------------------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Load  data ---------------------------

base::load("eda.RData")


# TAB 3: GLM, outcome: substance use, COVs: CDC guideline adherence ---------------------------

tab3_covs <- cov_dt %>%
  select(gender_3cat, ethnicity, essential_worker, income,
         covid_test, 
         daily_drinking, daily_opioid
  )

tab3_lm <- lm(data=tab3_covs, cdc_avg_out ~ .)
summary(tab3_lm)


# TAB 4: Logistic regression of SU vs any COVID-19 testing ---------------------------

tab4_covs <-cov_dt %>%
  select(age, education, ethnicity,
         race_4cat,
         essential_worker,
         income,
         household_size,
         daily_opioid
  )
dim(tab4_covs)
tab4_lm <- glm(data=tab4_covs, tested_for_covid ~ .,
               family = binomial(link = "logit")
)
tab4_summary <- summary(tab4_lm)

as.matrix(tab4_summary$coefficients)
exp(as.matrix(tab4_summary$coefficients[,1:2]))


# TAB 5: LR, outcome stimulant use w/ +COVID-19 test, ---------------------------

tab5_covs <-
  tested_for_covid_dt %>%
  select(ethnicity,
         dwelling_ownership,
         household_size,
         daily_stimulant
  )

tab5_lm <- glm(data=tab5_covs, tab5_outcome ~ .,
               family = binomial(link = "logit")
               )
tab5_summary <- summary(tab5_lm)

as.matrix(tab5_summary$coefficients)
exp(as.matrix(tab5_summary$coefficients[,1:2]))


# Create dataset of participant attributes needed for later network analysis


participant_dt_wide <- 
  cov_dt %>%
  select(age,
         race_4cat,
         gender_3cat,
         education,
         ethnicity, 
         essential_worker, 
         income,
         covid_test, 
         daily_drinking, 
         daily_opioid,
         daily_stimulant
         
  )

participant_dt_wide$MTURK1 <- dt$MTURK1
participant_dt_wide$cdc_avg_out <- cdc_avg_out
participant_dt_wide$CDC1 <- dt$CDC1
participant_dt_wide$CDC2 <- dt$CDC2
participant_dt_wide$CDC3 <- dt$CDC3
participant_dt_wide$CDC4 <- dt$CDC4
participant_dt_wide$CDC5 <- dt$CDC5
participant_dt_wide$CDC6 <- dt$CDC6
participant_dt_wide$CDC7 <- dt$CDC7
participant_dt_wide$CDC8 <- dt$CDC8
participant_dt_wide$CDC9 <- dt$CDC9
participant_dt_wide$CDC10 <- dt$CDC10
participant_dt_wide$CDC11 <- dt$CDC11
participant_dt_wide$CDC12 <- dt$CDC12
participant_dt_wide$CDC13 <- dt$CDC13

participant_dt_wide$FUSNCONSENT <- dt$FUSNCONSENT

dim(participant_dt_wide)
colnames(participant_dt_wide)

participant_dt_wide <- 
  participant_dt_wide %>%
  relocate(MTURK1, FUSNCONSENT)


# Save RDS --------
saveRDS(participant_dt_wide, file="participant_dt_wide.RDS")
