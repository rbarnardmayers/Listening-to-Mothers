
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/SetUp.R")
setwd("~/Documents/2025-2026/LTM")

# Source != "#NULL!" & Q2Source != "#NULL!" ~ "Q1 & Q2"

LTM <- read.csv("Data_FINAL12.csv")
LTM <- LTM %>% 
  mutate(which_surv = case_when(Q2Source == "#NULL!" ~ "Q1 only",
                                Q2Source != "#NULL!" ~ "Q2"),
         AIAN_NHPI = case_when(RACEC5 == 1.00 ~ "AIAN-NHPI",
                               RACEC7 == 1.00 ~ "AIAN-NHPI", 
                               TRUE ~ "Not"))