source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Data_Cleaning_1.R")

LTM2 <- LTM2 %>% 
  mutate(MH2WK_ANXDEP = case_when(PHQ4_MH2WK_ANX == 1 ~ 1, 
                                  PHQ4_MH2WK_DEP == 1 ~ 1, 
                                  TRUE ~ 0))


