# Compute characteristics of network participants when the egos are split into 
# quartiles by average CDC score.


# Top matter ----------

rm(list=ls())

install.packages("tidyverse")
library(tidyverse)
library(haven)
library(dplyr)


# Read data ----------

data_loc <- "/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT/"

sns_consenting_dt <- readRDS(paste0(data_loc, "sns_consenting_dt.RDS"))
dim(sns_consenting_dt)

sns_dt_long_wide_no_minors <- readRDS(paste0(data_loc, "sns_dt_long_wide_no_minors.RDS"))
dim(sns_dt_long_wide_no_minors)

sns_dt_long_wide_no_minors


# Classify egos into quartiles based on their CDC score ----------



sns_consenting_dt <- sns_consenting_dt %>%
  #Divides the dataset is divided into 4 roughly equal-sized groups,
  #and computes quartiles based on the max value of each group
  # some overlap between categories is possible
  # what we were doing previously
  mutate(quartile_old = ntile(cdc_avg_out, 4))

sns_consenting_dt <- sns_consenting_dt %>%
  # assign quartile values based on the distribution of cdc_avg_out,
  # uses data percentiles to categorize each value into its respective quartile,
  # leading to distinct but potentially unequal groups.
  mutate(
    quartile = case_when(
      cdc_avg_out <= quantile(cdc_avg_out, 0.25) ~ 1,
      cdc_avg_out <= quantile(cdc_avg_out, 0.50) ~ 2,
      cdc_avg_out <= quantile(cdc_avg_out, 0.75) ~ 3,
      TRUE ~ 4
    )
  )

cutoffs <- sns_consenting_dt %>%
  group_by(quartile) %>%
  summarize(
    min_value = min(cdc_avg_out),
    max_value = max(cdc_avg_out)
  )

print(cutoffs)

cutoffs_old <- sns_consenting_dt %>%
  group_by(quartile_old) %>%
  summarize(
    min_value = min(cdc_avg_out),
    max_value = max(cdc_avg_out)
  )
print(cutoffs_old)

quantile(sns_consenting_dt$cdc_avg_out, probs=c(0.25, 0.5, 0.75, 1))

table(sns_consenting_dt$quartile, exclude = NULL)
table(sns_consenting_dt$quartile_old, exclude = NULL)

# Conclusion:
## There will be more overlaps in the categories with the ntile function.
## The categorizations will not overlap with the quartile function. 
## Given that we are interested in classifying the egos into 4 clearly distinct
## groups, we will use the `quartile` method. 

# Join the ego and alter datasets using the MTURK1 and MTURKID columns: ----------

combined_data <- left_join(sns_dt_long_wide_no_minors, sns_consenting_dt, 
                           by = c("MTURKID" = "MTURK1"))

combined_data

table(combined_data$quartile, exclude = NULL)


# Sumarize age --------------------------------------

## summary
age_summary <- combined_data %>%
  group_by(quartile) %>%
  summarise(
    n = n(),
    n_missing = sum(is.na(SN2)),
    mean_age = mean(SN2, na.rm = TRUE), 
    sd_age = sd(SN2, na.rm = TRUE)
  )

age_summary

## test
age_test <- aov(SN2 ~ quartile, data = combined_data)
summary(age_test)


# Summarize gender ------------------------------------

## summary 
gender_distribution <- combined_data %>%
  group_by(quartile, SN4) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

gender_distribution_counts <- combined_data %>%
  group_by(quartile, SN4) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = SN4, values_from = n, values_fill = 0, names_prefix = "count_")

# Proportions by quartile and gender
gender_distribution_prop <- gender_distribution %>%
  tidyr::pivot_wider(names_from = SN4, 
                     values_from = proportion, 
                     values_fill = 0, names_prefix = "prop_")

gender_wide <- gender_distribution %>%
  tidyr::pivot_wider(names_from = SN4, 
                       values_from = c(proportion), values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

gender_wide <- inner_join(gender_distribution_counts, 
                          gender_distribution_prop, by = "quartile")

print(gender_wide)
  
## test

gender_contingency_table <- table(combined_data$quartile, combined_data$SN4)
print(gender_contingency_table)

gender_test <- chisq.test(gender_contingency_table)
print(gender_test)

#Summarize for ethnicity---------------------------

## summary 
ethnicity_distribution <- combined_data %>%
  group_by(quartile, SN5) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

# Raw counts by quartile and ethnicity
ethnicity_distribution_counts <- combined_data %>%
  group_by(quartile, SN5) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = SN5, values_from = n, values_fill = 0, names_prefix = "count_")

# Proportions by quartile and ethnicity
ethnicity_distribution_prop <- ethnicity_distribution %>%
  tidyr::pivot_wider(names_from = SN5, values_from = proportion, values_fill = 0, names_prefix = "prop_")

# Join counts and proportions
ethnicity_wide <- inner_join(ethnicity_distribution_counts, ethnicity_distribution_prop, by = "quartile")

print(ethnicity_wide)


## test

ethnicity_contingency_table <- table(combined_data$quartile, combined_data$SN5)
print(ethnicity_contingency_table)

ethnicity_test <- chisq.test(ethnicity_contingency_table)
print(ethnicity_test)

#Summarize for education ---------------

## recode
combined_data <- combined_data %>%
  mutate(
    SN6_recode = case_when(
      SN6 %in% c(1, 2, 3) ~ 1,
      SN6 == 4 ~ 4,
      SN6 %in% c(5, 6) ~ 5,
      TRUE ~ NA_integer_ # This captures all other values, including NAs. Adjust if needed.
    )
  )


# Raw counts by quartile and education
education_distribution_counts <- combined_data %>%
  group_by(quartile, SN6_recode) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = SN6_recode, values_from = n, values_fill = 0, names_prefix = "count_")

# Proportions by quartile and education
education_distribution_prop <- combined_data %>%
  group_by(quartile, SN6_recode) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n)) %>%
  tidyr::pivot_wider(names_from = SN6_recode, values_from = proportion, values_fill = 0, names_prefix = "prop_")

# Join counts and proportions
education_wide <- inner_join(education_distribution_counts, education_distribution_prop, by = "quartile")

print(education_wide)

## test

education_contingency_table <- table(combined_data$quartile, combined_data$SN6_recode)
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
  tidyr::pivot_wider(names_from = SN27, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(covid_test_wide)

## test

covid_test_contingency_table <- table(combined_data$quartile, combined_data$SN27)
print(covid_test_contingency_table)

covid_test_chi_test <- chisq.test(covid_test_contingency_table)
print(covid_test_chi_test)


#Summarize for Covid test (positive) -----------------------------------------------

# Create summary statistics by quartile and SN27
covid_test_distribution <- combined_data %>%
  group_by(quartile, SN27) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

# Pivot data to a wide format
covid_test_wide <- covid_test_distribution %>%
  tidyr::pivot_wider(names_from = SN27, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

# Print the wide-format table
print(covid_test_wide)

# Filter data for SN27 = 1 from the original distribution
filtered_SN27_1_distribution <- covid_test_distribution %>%
  filter(SN27 == 1)

# Print the filtered distribution summary
print(filtered_SN27_1_distribution)

# Filter the data for SN27 = 1 and SN27 != 1
filtered_data <- combined_data %>%
  mutate(SN27_filtered = if_else(SN27 == 1, "1", "not 1"))

# Create a new contingency table for the filtered data
filtered_covid_test_contingency_table <- table(filtered_data$quartile, filtered_data$SN27_filtered)
print(filtered_covid_test_contingency_table)

# Perform Chi-square test on the new contingency table
filtered_covid_test_chi_test <- chisq.test(filtered_covid_test_contingency_table)
print(filtered_covid_test_chi_test)



#Summarize for annual household income-------------------

## recode
combined_data <- combined_data %>%
  mutate(
    income_category = case_when(
      DEMO20 == 4 ~ "Up to $25,000",
      DEMO20 %in% c(5, 6) ~ "$25,000 - $50,000",
      DEMO20 %in% c(7, 8, 9, 10, 11) ~ "Above $50,000",
      TRUE ~ as.character(DEMO20) # This will keep any other categories or NAs as they are. Adjust if needed.
    )
  )

table(combined_data$income_category, exclude = NULL)

## summary 
annual_household_distribution <- combined_data %>%
  group_by(quartile, income_category) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n),
         n_missing = sum(is.na(income_category))) %>%
  arrange(quartile, 
          factor(income_category, levels=c("Up to $25,000", 
                                           "$25,000 - $50,000",
                                           "Above $50,000"))
          )

print(annual_household_distribution)


## test

annual_household_contingency_table <- table(combined_data$quartile, 
                                            combined_data$income_category)
annual_household_contingency_table <- annual_household_contingency_table[ , 
                                                                          c("Up to $25,000", 
                                                                            "$25,000 - $50,000", 
                                                                            "Above $50,000")]
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
  tidyr::pivot_wider(names_from = LSQ3, values_from = proportion, values_fill = 0) %>%
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
    sd_household = sd(DEMO8, na.rm = TRUE),
    n_missing = sum(is.na(DEMO8))
  )

household_size_summary

## test
household_size_test <- aov(DEMO8 ~ quartile, data = combined_data)
summary(household_size_test)

#Summarize for Political Party ------------------------------

head(combined_data$SN9)
table(combined_data$SN9, exclude = NULL)

## Recode
combined_data <- combined_data %>%
  mutate(
    SN9_recoded = case_when(
      SN9 == 1 ~ "Republican",
      SN9 == 2 ~ "Democrat",
      SN9 %in% c(3, 4, 5, 6, 7) ~ "Independent/Other",
      # Grouping Libertarian, Green, Other, and Prefer not to answer into "Other"
      is.na(SN9) ~ NA_character_          # Preserving NAs
    ),
    SN9_recoded = factor(SN9_recoded, 
                         levels = c("Democrat", "Republican", "Independent/Other"))
  )

table(combined_data$SN9_recoded, exclude = NULL)
table(combined_data$SN9_recoded, exclude = NULL)/sum(table(combined_data$SN9_recoded, exclude = NULL)
)

## Summary 
political_party_distribution <- combined_data %>%
  group_by(quartile, SN9_recoded) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))


political_party_wide <- political_party_distribution %>%
  tidyr::pivot_wider(names_from = SN9_recoded, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(political_party_wide)

## Test
political_party_contingency_table <- table(combined_data$quartile, 
                                           combined_data$SN9_recoded, 
                                           exclude = NULL)
print(political_party_contingency_table)

political_party_test <- chisq.test(political_party_contingency_table)
print(political_party_test)

combined_data$SN27a1


#Summarize for modes of communication -----------------------

# Convert the data into long format
combined_data_long <- combined_data %>%
  tidyr :: pivot_longer(
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
  tidyr::pivot_wider(names_from = SN10_type, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN11, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN12, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN13, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN14, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN15, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN16, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(spouse_or_partner_wide)

## Test
spouse_or_partner_contingency_table <- 
  table(combined_data$quartile, combined_data$SN16, exclude = NULL)
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
  tidyr::pivot_wider(names_from = SN17, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN18, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN19, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN20, values_from = proportion, values_fill = 0) %>%
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
  tidyr :: pivot_wider(names_from = SN21, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN22, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN22a, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN23, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN25, values_from = proportion, values_fill = 0) %>%
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
  tidyr::pivot_wider(names_from = SN26, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_to_use_drugs_wide)

## Test
encouraged_to_use_drugs_contingency_table <- table(combined_data$quartile, combined_data$SN26)
print(encouraged_to_use_drugs_contingency_table)

encouraged_to_use_drugs_test <- chisq.test(encouraged_to_use_drugs_contingency_table)
print(encouraged_to_use_drugs_test)

# Summarize for tested_for_covid -------------

head(combined_data$SN27)
table(combined_data$SN27, exclude = NULL)

## Summary 
tested_for_covid_distribution <- combined_data %>%
  group_by(quartile, SN27) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

tested_for_covid_wide <- tested_for_covid_distribution %>%
  tidyr::pivot_wider(names_from = SN27, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(tested_for_covid_wide)

## Test
tested_for_covid_contingency_table <- 
  table(combined_data$quartile, combined_data$SN27, exclude = NULL)
print(tested_for_covid_contingency_table)

tested_for_covid_test <- chisq.test(tested_for_covid_contingency_table)
print(tested_for_covid_test)

# Summarize for tested_positive_for_covid -------------
head(combined_data$SN27a)
table(combined_data$SN27a, exclude = NULL)

## Summary 
tested_positive_distribution <- combined_data %>%
  filter(!is.na(SN27a)) %>% # ignore NAs because skip pattern imposed by SN27
  group_by(quartile, SN27a) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

tested_positive_wide <- tested_positive_distribution %>%
  tidyr::pivot_wider(names_from = SN27a, 
                     values_from = proportion, values_fill = 0)

print(tested_positive_wide)

## Test
tested_positive_contingency_table <- 
  table(combined_data$quartile, combined_data$SN27a)
print(tested_positive_contingency_table)

tested_positive_test <- chisq.test(tested_positive_contingency_table)
print(tested_positive_test)


# Summarize for hospitalized_for_covid -------------
table(combined_data$SN27a1, exclude = NULL)
head(combined_data$SN27a1)

## Summary 
hospitalized_for_covid_distribution <- combined_data %>%
  filter(!is.na(SN27a)) %>% # ignore NAs because skip pattern imposed by SN27
  group_by(quartile, SN27a1) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

hospitalized_for_covid_wide <- hospitalized_for_covid_distribution %>%
  tidyr::pivot_wider(names_from = SN27a1, values_from = proportion, values_fill = 0)

print(hospitalized_for_covid_wide)

## Test
hospitalized_for_covid_contingency_table <- table(combined_data$quartile, 
                                                  combined_data$SN27a1)
print(hospitalized_for_covid_contingency_table)

hospitalized_for_covid_test <- chisq.test(hospitalized_for_covid_contingency_table)
print(hospitalized_for_covid_test)


# Summarize for knows_anyone_hospitalized_for_covid -------------
table(combined_data$SN28, exclude = NULL)
head(combined_data$SN28)

## Summary 
knows_anyone_hospitalized_for_covid_distribution <- combined_data %>%
  group_by(quartile, SN28) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

knows_anyone_hospitalized_for_covid_wide <- knows_anyone_hospitalized_for_covid_distribution %>%
  tidyr::pivot_wider(names_from = SN28, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(knows_anyone_hospitalized_for_covid_wide)

## Test
knows_anyone_hospitalized_for_covid_contingency_table <- 
  table(combined_data$quartile, combined_data$SN28, exclude = NULL)
print(knows_anyone_hospitalized_for_covid_contingency_table)

knows_anyone_hospitalized_for_covid_test <- chisq.test(knows_anyone_hospitalized_for_covid_contingency_table)
print(knows_anyone_hospitalized_for_covid_test)


# Summarize for encouraged_testing_for_covid -------------
head(combined_data$SN32)
table(combined_data$SN32, exclude = NULL)

## Summary 
encouraged_testing_for_covid_distribution <- combined_data %>%
  group_by(quartile, SN32) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_testing_for_covid_wide <- encouraged_testing_for_covid_distribution %>%
  tidyr::pivot_wider(names_from = SN32, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_testing_for_covid_wide)

## Test
encouraged_testing_for_covid_contingency_table <- 
  table(combined_data$quartile, combined_data$SN32, exclude = NULL)
print(encouraged_testing_for_covid_contingency_table)

encouraged_testing_for_covid_test <- chisq.test(encouraged_testing_for_covid_contingency_table)
print(encouraged_testing_for_covid_test)

# Summarize for follows_social_distancing -------------
head(combined_data$SN33)
table(combined_data$SN33, exclude = NULL)


## Summary 
follows_social_distancing_distribution <- combined_data %>%
  group_by(quartile, SN33) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

follows_social_distancing_wide <- follows_social_distancing_distribution %>%
  tidyr::pivot_wider(names_from = SN33, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(follows_social_distancing_wide)

## Test
follows_social_distancing_contingency_table <- 
  table(combined_data$quartile, combined_data$SN33, exclude = NULL)
print(follows_social_distancing_contingency_table)

follows_social_distancing_test <- chisq.test(follows_social_distancing_contingency_table)
print(follows_social_distancing_test)


# Summarize for encouraged_social_distancing -------------
head(combined_data$SN34)
table(combined_data$SN34, exclude = NULL)

## Summary 
encouraged_social_distancing_distribution <- combined_data %>%
  group_by(quartile, SN34) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_social_distancing_wide <- encouraged_social_distancing_distribution %>%
  tidyr::pivot_wider(names_from = SN34, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_social_distancing_wide)

## Test
encouraged_social_distancing_contingency_table <- table(combined_data$quartile, combined_data$SN34)
print(encouraged_social_distancing_contingency_table)

encouraged_social_distancing_test <- chisq.test(encouraged_social_distancing_contingency_table)
print(encouraged_social_distancing_test)

# Summarize for received_covid_vaccine -------------
head(combined_data$SN37)
table(combined_data$SN37, exclude = NULL)

## Summary 
received_covid_vaccine_distribution <- combined_data %>%
  group_by(quartile, SN37) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

received_covid_vaccine_wide <- received_covid_vaccine_distribution %>%
  tidyr::pivot_wider(names_from = SN37, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(received_covid_vaccine_wide)

## Test
received_covid_vaccine_contingency_table <- 
  table(combined_data$quartile, combined_data$SN37, exclude = NULL)
print(received_covid_vaccine_contingency_table)

received_covid_vaccine_test <- chisq.test(received_covid_vaccine_contingency_table)
print(received_covid_vaccine_test)

# Summarize for vaccine_side_effects -------------
## Summary 
head(combined_data$SN37a)
table(combined_data$SN37a, exclude = NULL)

vaccine_side_effects_distribution <- combined_data %>%
  group_by(quartile, SN37a) %>%
  filter(!is.na(SN37a)) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

vaccine_side_effects_wide <- vaccine_side_effects_distribution %>%
  tidyr::pivot_wider(names_from = SN37a, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(vaccine_side_effects_wide)

## Test
vaccine_side_effects_contingency_table <- table(combined_data$quartile, combined_data$SN37a)
print(vaccine_side_effects_contingency_table)

vaccine_side_effects_test <- chisq.test(vaccine_side_effects_contingency_table)
print(vaccine_side_effects_test)


# Summarize for open_to_vaccine -------------
## Summary 
open_to_vaccine_distribution <- combined_data %>%
  group_by(quartile, SN37b) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

open_to_vaccine_wide <- open_to_vaccine_distribution %>%
  tidyr::pivot_wider(names_from = SN37b, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(open_to_vaccine_wide)

## Test
open_to_vaccine_contingency_table <- table(combined_data$quartile, combined_data$SN37b)
print(open_to_vaccine_contingency_table)

open_to_vaccine_test <- chisq.test(open_to_vaccine_contingency_table)
print(open_to_vaccine_test)


# Summarize for encouraged_vaccine -------------
## Summary 
encouraged_vaccine_distribution <- combined_data %>%
  group_by(quartile, SN38) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

encouraged_vaccine_wide <- encouraged_vaccine_distribution %>%
  tidyr::pivot_wider(names_from = SN38, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(encouraged_vaccine_wide)

## Test
encouraged_vaccine_contingency_table <- table(combined_data$quartile, combined_data$SN38)
print(encouraged_vaccine_contingency_table)

encouraged_vaccine_test <- chisq.test(encouraged_vaccine_contingency_table)
print(encouraged_vaccine_test)



# Summarize for discouraged_vaccine -------------
## Summary 
discouraged_vaccine_distribution <- combined_data %>%
  group_by(quartile, SN39) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

discouraged_vaccine_wide <- discouraged_vaccine_distribution %>%
  tidyr::pivot_wider(names_from = SN39, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

print(discouraged_vaccine_wide)

## Test
head(combined_data$SN39)
discouraged_vaccine_contingency_table <- table(combined_data$quartile, combined_data$SN39)
print(discouraged_vaccine_contingency_table)

discouraged_vaccine_test <- chisq.test(discouraged_vaccine_contingency_table)
print(discouraged_vaccine_test)

# Save data

network_contact_data_env <- new.env()
network_contact_data_env$combined_data <- combined_data

##summary statistics political affilation ------------------------------
SN9_distribution <- combined_data %>%
  group_by(quartile, SN9) %>%
  tally() %>%
  group_by(quartile) %>%
  mutate(proportion = n/sum(n))

# Pivot data to a wide format for SN9
SN9_wide <- SN9_distribution %>%
  tidyr::pivot_wider(names_from = SN9, values_from = proportion, values_fill = 0) %>%
  group_by(quartile) %>%
  summarise(across(everything(), sum))

# Print the wide-format table for SN9
print(SN9_wide)

# Create a contingency table for SN9
SN9_contingency_table <- table(combined_data$quartile, combined_data$SN9)
print(SN9_contingency_table)

# Perform a Chi-square test for SN9
SN9_chi_test <- chisq.test(SN9_contingency_table)
print(SN9_chi_test)

##Summarize for race ----------------------------------------
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

# Create a contingency table with quartile as rows and sum of values across all SN3_types
contingency_table <- with(combined_data_long, table(quartile, value))

# Perform chi-square test
test_result <- chisq.test(contingency_table)

# Print p-value
print(test_result$p.value)




saveRDS(network_contact_data_env, paste0(data_loc, 
                                         "network_contact_data_objects.rds"))

