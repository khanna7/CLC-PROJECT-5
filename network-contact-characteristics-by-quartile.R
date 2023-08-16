# Compute characteristics of network participants when the egos are split into 
# quartiles by average CDC score.


# Top matter ----------

rm(list=ls())

library(haven)
library(dplyr)
library(data.table)
library(tidyverse)


# Read data ----------

data_loc <- "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT/"

sns_consenting_dt <- readRDS(paste0(data_loc, "sns_consenting_dt.RDS"))
dim(sns_consenting_dt)

sns_dt_long_wide_no_minors <- readRDS(paste0(data_loc, "sns_dt_long_wide_no_minors.RDS"))
dim(sns_dt_long_wide_no_minors)


# Classify egos into quartiles based on their CDC score ----------

sns_consenting_dt <- sns_consenting_dt %>%
  mutate(quartile = ntile(cdc_avg_out, 4))

# Join the ego and alter datasets using the MTURK1 and MTURKID columns: ----------

combined_data <- left_join(sns_dt_long_wide_no_minors, sns_consenting_dt, 
                           by = c("MTURKID" = "MTURK1"))


# Sumarize age ---------

## summary
age_summary <- combined_data %>%
  group_by(quartile) %>%
  summarise(
    mean_age = mean(SN2, na.rm = TRUE), 
    sd_age = sd(SN2, na.rm = TRUE)
  )

age_summary

## test
age_test <- aov(SN2 ~ quartile, data = combined_data)
summary(age_test)


# Summarize gender ---------

## summary 
gender_distribution <- combined_data %>%
  group_by(quartile, SN4) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

gender_wide <- gender_distribution %>%
  pivot_wider(names_from = SN4, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(`1`:`NA`, sum))

print(gender_wide)

  
## test

gender_contingency_table <- table(combined_data$quartile, combined_data$SN4)
print(gender_contingency_table)

gender_test <- chisq.test(gender_contingency_table)
print(gender_test)

