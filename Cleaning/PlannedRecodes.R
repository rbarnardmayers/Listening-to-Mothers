# Recoding Planned and Unplanned
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Data Cleaning.R")
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers")

types <- read.csv("CSECTIONTYPES.csv") %>% 
  mutate(CSECTIONTYPE_M = case_when(CSECTIONTYPE_M == 3 ~ 95, 
                                   TRUE ~ CSECTIONTYPE_M)) #%>% 
  # rename(CSECTIONTYPE = CSECTIONTYPE_M)
planned <- read.csv("PLANNEDREASON.csv") %>% 
  mutate(PLANNEDREASON = case_when(PLANNEDREASON == 9 ~ 95, 
                                       TRUE ~ PLANNEDREASON)) #%>% 
  # rename(PLANNEDC = PLANNEDREASON)
unplanned <- read.csv("UNPLANNEDREASON.csv") %>% 
  mutate(UNPLANNEDREASON_M = case_when(UNPLANNEDREASON_M == 5 ~ 95, 
                                       TRUE ~ UNPLANNEDREASON_M)) #%>% 
  # rename(UNPLANNEDREASON = UNPLANNEDREASON_M)

LTM2 <- LTM2 %>% 
  full_join(types) %>% 
  full_join(planned) %>% 
  full_join(unplanned) %>% 
  mutate(CSECTIONTYPE = case_when(CSECTIONTYPE == 95 ~ CSECTIONTYPE_M,
                                  TRUE ~ CSECTIONTYPE),
         UNPLANNEDREASON = case_when(UNPLANNEDREASON == 95 ~ UNPLANNEDREASON_M, 
                                     TRUE ~ UNPLANNEDREASON),
         PLANNEDC = case_when(PLANNEDC == 95 ~ PLANNEDREASON, 
                                     TRUE ~ PLANNEDC),
         CSECTIONTYPE_R = case_when(CSECTIONTYPE == 2 ~ "Unplanned C-section", 
                                    CSECTIONTYPE == 1 | CSECTIONTYPE == 95 ~ "Not Unplanned C-section",
                                    MODE2023 == 1 ~ "Vaginal birth"), 
         CSECTIONTYPE_R2 = case_when(CSECTIONTYPE == 2 ~ "Unplanned C-section", 
                                     CSECTIONTYPE == 1 ~ "Planned C-section",
                                     CSECTIONTYPE == 95 ~ "Other",
                                     MODE2023 == 1 ~ "Vaginal birth"))
