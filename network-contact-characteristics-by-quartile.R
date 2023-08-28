# Compute characteristics of network participants when the egos are split into 
# quartiles by average CDC score.

rm(list=ls())
knitr::opts_knit$set(root.dir = "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


rm(list=ls())
library(dplyr)


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

sns_dt_long_wide_no_minors
# Classify egos into quartiles based on their CDC score ----------

sns_consenting_dt <- sns_consenting_dt %>%
  mutate(quartile = ntile(cdc_avg_out, 4))



# Join the ego and alter datasets using the MTURK1 and MTURKID columns: ----------

combined_data <- left_join(sns_dt_long_wide_no_minors, sns_consenting_dt, 
                           by = c("MTURKID" = "MTURK1"))

combined_data

table(combined_data$quartile)


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
  summarise(across(everything(), sum))

print(gender_wide)

  
## test

gender_contingency_table <- table(combined_data$quartile, combined_data$SN4)
print(gender_contingency_table)

gender_test <- chisq.test(gender_contingency_table)
print(gender_test)

# Summarize race -----------

combined_data_long <- combined_data %>%
  pivot_longer(cols = starts_with("SN3_"), 
               names_to = "SN3_type", 
               values_to = "value")
# Summary
race_distribution <- combined_data_long %>%
  group_by(quartile, SN3_type, value) %>%
  tally() %>%
  group_by(quartile, SN3_type) %>%
  mutate(proportion = n/sum(n))

race_wide <- race_distribution %>%
  pivot_wider(names_from = value, values_from = proportion, values_fill = 0) %>%
  group_by(quartile, SN3_type) %>%
  summarise(across(everything(), sum))  

race_wide <- race_wide %>%
  arrange(SN3_type, quartile)

print(race_wide, n = Inf)

#test

library(dplyr)
library(tidyr)
library(purrr)


# Convert the data into long format
combined_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("SN3_"), 
    names_to = "SN3_type", 
    values_to = "value"
  )

# Sum across the SN3_type columns for each quartile
# Sum across the SN3_type columns for each quartile
combined_counts <- combined_data_long %>%
  group_by(quartile) %>%
  summarise(across(starts_with("SN3_"), sum))


# Create a contingency table with quartile as rows and sum of values across all SN3_types
contingency_table <- with(combined_data_long, table(quartile, value))

# Perform chi-square test
test_result <- chisq.test(contingency_table)

# Print p-value
print(test_result$p.value)

#Summarize for ethnicity

## summary 
ethnicity_distribution <- combined_data %>%
  group_by(quartile, SN5) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

ethnicity_wide <- ethnicity_distribution %>%
  pivot_wider(names_from = SN5, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(ethnicity_wide)

## test

ethnicity_contingency_table <- table(combined_data$quartile, combined_data$SN5)
print(ethnicity_contingency_table)

ethnicity_test <- chisq.test(ethnicity_contingency_table)
print(ethnicity_test)

#Summarize for education ---------------

## summary 
education_distribution <- combined_data %>%
  group_by(quartile, SN6) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

education_wide <- education_distribution %>%
  pivot_wider(names_from = SN6, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(education_wide)

## test

education_contingency_table <- table(combined_data$quartile, combined_data$SN6)
print(education_contingency_table)

education_test <- chisq.test(education_contingency_table)
print(education_test)


#Summarize for covid test ------------------
## summary 
covid_test_distribution <- combined_data %>%
  group_by(quartile, SN27) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

covid_test_wide <- covid_test_distribution %>%
  pivot_wider(names_from = SN27, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(covid_test_wide)

## test

covid_test_contingency_table <- table(combined_data$quartile, combined_data$SN27)
print(covid_test_contingency_table)

covid_test_chi_test <- chisq.test(covid_test_contingency_table)
print(covid_test_chi_test)

#Summarize for annual household income-------------------
## summary 
annual_household_distribution <- combined_data %>%
  group_by(quartile, DEMO20) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

annual_household_wide <- annual_household_distribution %>%
  pivot_wider(names_from = DEMO20, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(annual_household_wide)

## test

annual_household_contingency_table <- table(combined_data$quartile, combined_data$DEMO20)
print(annual_household_contingency_table)

annual_household_chi_test <- chisq.test(annual_household_contingency_table)
print(annual_household_chi_test)


#Summarize for essential_worker -------------
## Summary 
essential_worker_distribution <- combined_data %>%
  group_by(quartile, LSQ3) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

essential_worker_wide <- essential_worker_distribution %>%
  pivot_wider(names_from = LSQ3, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(essential_worker_wide)

## Test
essential_worker_contingency_table <- table(combined_data$quartile, combined_data$LSQ3)
print(essential_worker_contingency_table)

essential_worker_test <- chisq.test(essential_worker_contingency_table)
print(essential_worker_test)

#Summarize for household size------------------------------

## summary

combined_data$DEMO8 <- as.numeric(as.character(combined_data$DEMO8))

household_size_summary <- combined_data %>%
  group_by(quartile) %>%
  summarise(
    mean_household = mean(DEMO8, na.rm = TRUE), 
    sd_household = sd(DEMO8, na.rm = TRUE)
  )

household_size_summary

## test
household_size_test <- aov(DEMO8 ~ quartile, data = combined_data)
summary(household_size_test)

#Summarize for covid_test positive-----------------

positive_test_summary <- combined_data %>%
  group_by(quartile) %>%
  summarise(
    mean_positive_test = mean(SN27a1, na.rm = TRUE), 
    sd_positive_test = sd(SN27a1, na.rm = TRUE)
  )

positive_test_summary

## test
positive_test <- aov(SN27a1 ~ quartile, data = combined_data)
summary(positive_test)

#Summarize for modes of communication -----------------------

# Convert the data into long format
combined_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("SN10_"), 
    names_to = "SN10_type", 
    values_to = "value"
  )

# Summarize by quartile for each SN10_type
communication_distribution <- combined_data_long %>%
  group_by(quartile, SN10_type) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

communication_wide <- communication_distribution %>%
  pivot_wider(names_from = SN10_type, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(communication_wide)

# Create the contingency table
communication_contingency_table <- table(combined_data_long$quartile, combined_data_long$SN10_type)

# Perform chi-square test
communication_test <- chisq.test(communication_contingency_table)

# Print the test result
print(communication_test)


# Summarize for mode_of_communication -------------
## Summary 
mode_of_communication_distribution <- combined_data %>%
  group_by(quartile, SN11) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

mode_of_communication_wide <- mode_of_communication_distribution %>%
  pivot_wider(names_from = SN11, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(mode_of_communication_wide)

## Test
mode_of_communication_contingency_table <- table(combined_data$quartile, combined_data$SN11)
print(mode_of_communication_contingency_table)

mode_of_communication_test <- chisq.test(mode_of_communication_contingency_table)
print(mode_of_communication_test)

# Summarize for length_of_friendship -------------
## Summary 
length_of_friendship_distribution <- combined_data %>%
  group_by(quartile, SN12) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

length_of_friendship_wide <- length_of_friendship_distribution %>%
  pivot_wider(names_from = SN12, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(length_of_friendship_wide)

## Test
length_of_friendship_contingency_table <- table(combined_data$quartile, combined_data$SN12)
print(length_of_friendship_contingency_table)

length_of_friendship_test <- chisq.test(length_of_friendship_contingency_table)
print(length_of_friendship_test)

# Summarize for frequency_of_communication -------------
## Summary 
frequency_of_communication_distribution <- combined_data %>%
  group_by(quartile, SN13) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

frequency_of_communication_wide <- frequency_of_communication_distribution %>%
  pivot_wider(names_from = SN13, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(frequency_of_communication_wide)

## Test
frequency_of_communication_contingency_table <- table(combined_data$quartile, combined_data$SN13)
print(frequency_of_communication_contingency_table)

frequency_of_communication_test <- chisq.test(frequency_of_communication_contingency_table)
print(frequency_of_communication_test)

# Summarize for discuss_personal_matters -------------
## Summary 
discuss_personal_matters_distribution <- combined_data %>%
  group_by(quartile, SN14) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

discuss_personal_matters_wide <- discuss_personal_matters_distribution %>%
  pivot_wider(names_from = SN14, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(discuss_personal_matters_wide)

## Test
discuss_personal_matters_contingency_table <- table(combined_data$quartile, combined_data$SN14)
print(discuss_personal_matters_contingency_table)

discuss_personal_matters_test <- chisq.test(discuss_personal_matters_contingency_table)
print(discuss_personal_matters_test)

# Summarize for ask_for_advice_on_personal_matters -------------
## Summary 
ask_for_advice_distribution <- combined_data %>%
  group_by(quartile, SN15) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

ask_for_advice_wide <- ask_for_advice_distribution %>%
  pivot_wider(names_from = SN15, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(ask_for_advice_wide)

## Test
ask_for_advice_contingency_table <- table(combined_data$quartile, combined_data$SN15)
print(ask_for_advice_contingency_table)

ask_for_advice_test <- chisq.test(ask_for_advice_contingency_table)
print(ask_for_advice_test)

# Summarize for spouse_or_intimate_partner -------------
## Summary 
spouse_or_partner_distribution <- combined_data %>%
  group_by(quartile, SN16) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

spouse_or_partner_wide <- spouse_or_partner_distribution %>%
  pivot_wider(names_from = SN16, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(spouse_or_partner_wide)

## Test
spouse_or_partner_contingency_table <- table(combined_data$quartile, combined_data$SN16)
print(spouse_or_partner_contingency_table)

spouse_or_partner_test <- chisq.test(spouse_or_partner_contingency_table)
print(spouse_or_partner_test)

# Summarize for close_relative -------------
## Summary 
close_relative_distribution <- combined_data %>%
  group_by(quartile, SN17) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

close_relative_wide <- close_relative_distribution %>%
  pivot_wider(names_from = SN17, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(close_relative_wide)

## Test
close_relative_contingency_table <- table(combined_data$quartile, combined_data$SN17)
print(close_relative_contingency_table)

close_relative_test <- chisq.test(close_relative_contingency_table)
print(close_relative_test)

# Summarize for sexual_relationship -------------
## Summary 
sexual_relationship_distribution <- combined_data %>%
  group_by(quartile, SN18) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

sexual_relationship_wide <- sexual_relationship_distribution %>%
  pivot_wider(names_from = SN18, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(sexual_relationship_wide)

## Test
sexual_relationship_contingency_table <- table(combined_data$quartile, combined_data$SN18)
print(sexual_relationship_contingency_table)

sexual_relationship_test <- chisq.test(sexual_relationship_contingency_table)
print(sexual_relationship_test)

# Summarize for cohabiting -------------
## Summary 
cohabiting_distribution <- combined_data %>%
  group_by(quartile, SN19) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

cohabiting_wide <- cohabiting_distribution %>%
  pivot_wider(names_from = SN19, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(cohabiting_wide)

## Test
cohabiting_contingency_table <- table(combined_data$quartile, combined_data$SN19)
print(cohabiting_contingency_table)

cohabiting_test <- chisq.test(cohabiting_contingency_table)
print(cohabiting_test)

# Summarize for smokes_tobacco -------------
## Summary 
smokes_tobacco_distribution <- combined_data %>%
  group_by(quartile, SN20) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

smokes_tobacco_wide <- smokes_tobacco_distribution %>%
  pivot_wider(names_from = SN20, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(smokes_tobacco_wide)

## Test
smokes_tobacco_contingency_table <- table(combined_data$quartile, combined_data$SN20)
print(smokes_tobacco_contingency_table)

smokes_tobacco_test <- chisq.test(smokes_tobacco_contingency_table)
print(smokes_tobacco_test)

# Summarize for encouraged_to_smoke_during_covid -------------
## Summary 
encouraged_to_smoke_distribution <- combined_data %>%
  group_by(quartile, SN21) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_to_smoke_wide <- encouraged_to_smoke_distribution %>%
  pivot_wider(names_from = SN21, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_to_smoke_wide)

## Test
encouraged_to_smoke_contingency_table <- table(combined_data$quartile, combined_data$SN21)
print(encouraged_to_smoke_contingency_table)

encouraged_to_smoke_test <- chisq.test(encouraged_to_smoke_contingency_table)
print(encouraged_to_smoke_test)

# Summarize for drinks_alcohol -------------
## Summary 
drinks_alcohol_distribution <- combined_data %>%
  group_by(quartile, SN22) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

drinks_alcohol_wide <- drinks_alcohol_distribution %>%
  pivot_wider(names_from = SN22, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(drinks_alcohol_wide)

## Test
drinks_alcohol_contingency_table <- table(combined_data$quartile, combined_data$SN22)
print(drinks_alcohol_contingency_table)

drinks_alcohol_test <- chisq.test(drinks_alcohol_contingency_table)
print(drinks_alcohol_test)

# Summarize for how_often_drinks_alcohol -------------
## Summary 
how_often_drinks_distribution <- combined_data %>%
  group_by(quartile, SN22a) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

how_often_drinks_wide <- how_often_drinks_distribution %>%
  pivot_wider(names_from = SN22a, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(how_often_drinks_wide)

## Test
how_often_drinks_contingency_table <- table(combined_data$quartile, combined_data$SN22a)
print(how_often_drinks_contingency_table)

how_often_drinks_test <- chisq.test(how_often_drinks_contingency_table)
print(how_often_drinks_test)

# Summarize for encouraged_to_drink_during_covid -------------
## Summary 
encouraged_to_drink_distribution <- combined_data %>%
  group_by(quartile, SN23) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_to_drink_wide <- encouraged_to_drink_distribution %>%
  pivot_wider(names_from = SN23, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_to_drink_wide)

## Test
encouraged_to_drink_contingency_table <- table(combined_data$quartile, combined_data$SN23)
print(encouraged_to_drink_contingency_table)

encouraged_to_drink_test <- chisq.test(encouraged_to_drink_contingency_table)
print(encouraged_to_drink_test)

# Summarize for uses_non_prescription_drugs -------------
## Summary 
uses_drugs_distribution <- combined_data %>%
  group_by(quartile, SN25) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

uses_drugs_wide <- uses_drugs_distribution %>%
  pivot_wider(names_from = SN25, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(uses_drugs_wide)

## Test
uses_drugs_contingency_table <- table(combined_data$quartile, combined_data$SN25)
print(uses_drugs_contingency_table)

uses_drugs_test <- chisq.test(uses_drugs_contingency_table)
print(uses_drugs_test)


# Summarize for encouraged_to_use_drugs -------------
## Summary 
encouraged_to_use_drugs_distribution <- combined_data %>%
  group_by(quartile, SN26) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_to_use_drugs_wide <- encouraged_to_use_drugs_distribution %>%
  pivot_wider(names_from = SN26, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_to_use_drugs_wide)

## Test
encouraged_to_use_drugs_contingency_table <- table(combined_data$quartile, combined_data$SN26)
print(encouraged_to_use_drugs_contingency_table)

encouraged_to_use_drugs_test <- chisq.test(encouraged_to_use_drugs_contingency_table)
print(encouraged_to_use_drugs_test)

# Summarize for tested_for_covid -------------
## Summary 
tested_for_covid_distribution <- combined_data %>%
  group_by(quartile, SN27) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

tested_for_covid_wide <- tested_for_covid_distribution %>%
  pivot_wider(names_from = SN27, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(tested_for_covid_wide)

## Test
tested_for_covid_contingency_table <- table(combined_data$quartile, combined_data$SN27)
print(tested_for_covid_contingency_table)

tested_for_covid_test <- chisq.test(tested_for_covid_contingency_table)
print(tested_for_covid_test)

# Summarize for tested_positive_for_covid -------------
## Summary 
tested_positive_distribution <- combined_data %>%
  group_by(quartile, SN27a) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

tested_positive_wide <- tested_positive_distribution %>%
  pivot_wider(names_from = SN27a, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(tested_positive_wide)

## Test
tested_positive_contingency_table <- table(combined_data$quartile, combined_data$SN27a)
print(tested_positive_contingency_table)

tested_positive_test <- chisq.test(tested_positive_contingency_table)
print(tested_positive_test)


# Summarize for hospitalized_for_covid -------------
## Summary 
hospitalized_for_covid_distribution <- combined_data %>%
  group_by(quartile, SN27a1) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

hospitalized_for_covid_wide <- hospitalized_for_covid_distribution %>%
  pivot_wider(names_from = SN27a1, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(hospitalized_for_covid_wide)

## Test
hospitalized_for_covid_contingency_table <- table(combined_data$quartile, combined_data$SN27a1)
print(hospitalized_for_covid_contingency_table)

hospitalized_for_covid_test <- chisq.test(hospitalized_for_covid_contingency_table)
print(hospitalized_for_covid_test)



