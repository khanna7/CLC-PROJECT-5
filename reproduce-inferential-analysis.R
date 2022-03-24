# REPRODUCE INFERENTIAL TABLES (3-5) FROM PRIOR PAPER

rm(list=ls())


# Set working directory ---------------------------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Load  data ---------------------------

load("eda.RData")


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
         # add race
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
## accounting for covariates, in the subset of participants
## reporting a COVID-19 test (n=279). 

tab5_covs <-cov_dt %>%
  select(ethnicity,
         # dwelling ownership
         essential_worker,
         income,
         household_size,
         daily_opioid
  )


