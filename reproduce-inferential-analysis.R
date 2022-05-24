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

