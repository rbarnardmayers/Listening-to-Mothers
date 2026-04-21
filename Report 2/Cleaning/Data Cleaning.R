source("~/Documents/2025-2026/LTM/Listening-to-Mothers/HelpfulFunctions.R")
setwd("~/Documents/2025-2026/LTM")

# Data Read in &  Get rid of identifying information ----
LTM <- read.csv("Data_FINAL2.csv")

LTM1 <- LTM %>% 
  mutate(MDID = as.character(MDID))

miss_vars <- dict2[dict2$missing == 99999,"variable"] %>% subset(!is.na(variable)) 

# Create dataset of just text responses and MDID (so we can merge back) ----
# Don't want to convert these columns to numeric
ends_o <- LTM1[str_ends(colnames(LTM1), "O")]  %>% colnames()

LTM_ignore <- LTM1 %>% 
  select(c(all_of(ends_o), MDID, #other vars we don't want
           )) 

# List of columns that are text only 
ignores <- LTM_ignore %>% 
  select(-c(MDID)) %>% 
  colnames() 

# Create dataset of the numeric columns to convert----
LTM_keep <- LTM1 %>% 
  select(-c(all_of(ignores)), MDID)

# Create list of column names to convert to numeric
tochange <- LTM_keep %>% 
  select(-c(MDID, LastConnectionDate)) %>%
  colnames()

# Convert all columns in LTM_keep to numeric
for(i in tochange){
  LTM_keep[[i]] <- as.numeric(LTM_keep[[i]])
}

# Complete dataset
LTM1 <- LTM_keep %>% 
  full_join(LTM_ignore, join_by(MDID))

# Creating new variables ----

LTM2 <- LTM1 %>% mutate()

LTM2 <- as.data.frame(LTM2)