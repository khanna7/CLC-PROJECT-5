# Compute characteristics of network participants when the egos are split into 
# quartiles by average CDC score.

```{r, setup, include=FALSE}
rm(list=ls())
knitr::opts_knit$set(root.dir = "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")
```

```{r, echo=FALSE, message=FALSE}
rm(list=ls())
library(dplyr)
```

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

# Perform the chi-square test for each SN3_type
test_results <- combined_data_long %>%
  group_by(SN3_type) %>%
  do(test_result = tryCatch(
    {
      test = chisq.test(table(.$quartile, .$value))
      data.frame(method = "Chi-squared", statistic = test$statistic, p_value = test$p.value)
    },
    error = function(e) {
      return(data.frame(method = "Error", statistic = NA, p_value = NA))
    }
  )) %>%
  unnest(cols = test_result)

print(test_results)


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


