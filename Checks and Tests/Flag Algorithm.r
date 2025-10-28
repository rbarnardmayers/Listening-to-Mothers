# Algorithm Application

source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Helpful_Functions.R")

# Data Read in &  Get rid of identifying information ----
LTM <- read.csv("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Data_9.4.25.csv") 


LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  group_by(F1) %>% 
  summarise(n = n())

LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>% 
  mutate(F2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  group_by(F2) %>% 
  summarise(n = n())

LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>% 
  mutate(F2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>%
  rowwise() %>%
  mutate(all = sum(FLAG2, FLAG3)) %>%
  mutate(F3 = case_when(all < 6 ~ 0, TRUE ~ 1)) %>%
  group_by(F3) %>% 
  summarise(n = n())

LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>% 
  mutate(F2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>%
  mutate(F3 = case_when(FLAG_SUM < 6 ~ 0, TRUE ~ 1)) %>%
  subset(F3 == 0) %>%
  group_by(FINAL_DETERMINATION) %>%
  summarise(n = n())

