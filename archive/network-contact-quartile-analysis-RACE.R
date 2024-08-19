# Compute characteristics of network participants when the egos are split into 
# quartiles by average CDC score -- RACE Distribution only


#load packages

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
  mutate(quartile = ntile(cdc_avg_out, 4))



# Join the ego and alter datasets using the MTURK1 and MTURKID columns: ----------

combined_data <- left_join(sns_dt_long_wide_no_minors, sns_consenting_dt, 
                           by = c("MTURKID" = "MTURK1"))

combined_data

table(combined_data$quartile, exclude = NULL)

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