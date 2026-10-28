# Requested Columns
library(readr)
library(dplyr)
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Data Cuts")

# Q1 cut ----
cutq1 <- read_xlsx("Q1_CUT.xlsx")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 1/Cleaning/ApplyDictionary.R")
cols_1 <- cutq1[cutq1$Columns != "BIRTHDATE_D",]$Columns
LTM_cut1 <- LTM_final %>% 
  select(all_of(cols_1))

colnames(LTM_cut1)

write.csv(LTM_cut1, "LTM_Q1_102826.csv")

# Q2 cut ----
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Data Cuts")
cutq2 <- read_xlsx("Q2_CUT.xlsx")
cols_2 <- cutq2[cutq2$Columns != "BIRTHDATE_D",]$Columns
LTM_cut2 <- LTM_final %>% 
  select(c(all_of(cols_2)))

colnames(LTM_cut2)

write.csv(LTM_cut2, "LTM_Q2_102826.csv")
