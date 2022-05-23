rm(list=ls())


# Set working directory ---------------------------

setwd("/Volumes/caas/CADRE CLC Data Project5/Clean Data/AK-SU-NETWORKS-ROUT")


# Load libraries ---------------------------

library(haven)
library(dplyr)


# Load individual data analysis ---------------------------

load("eda.RData")


# Read network data ---------------------------
network_dt <- read_sav("../Social Networks_ Substance Use and COVID Prevention.sav")


# Correct for missing data ---------------------------

## Do we need to do something similar here to what we did for the individual data? 
# See https://github.com/khanna7/CLC-PROJECT-5/blob/3cffa997dbaf166c57f4cae832f4dcf68db84c5d/EDA.R#L25



# Compute network statistics ---------------------------

## 
