source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Data_Cleaning_1.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/CS_Type.R")

LTM2 <- LTM2 %>% 
  mutate(MH2WK_ANXDEP = case_when(PHQ4_MH2WK_ANX == 1 ~ 1, 
                                  PHQ4_MH2WK_DEP == 1 ~ 1, 
                                  TRUE ~ 0),
         EMPLOYBABYAGE = as.numeric(EMPLOYBABYAGE),
         EMPLOYBABYAGE_R = case_when(EMPLOYBABYAGE <= 6 ~ "6 weeks or less",
                                     EMPLOYBABYAGE > 6 ~ "More than 6 weeks"),
         ANYMISTREAT = case_when(MISTREATC1 == 1 ~ "Mistreated",
                                 MISTREATC2 == 1 ~ "Mistreated",
                                 MISTREATC3 == 1 ~ "Mistreated",
                                 MISTREATC4 == 1 ~ "Mistreated",
                                 MISTREATC5 == 1 ~ "Mistreated",
                                 MISTREATC6 == 1 ~ "Mistreated",
                                 MISTREATC7 == 1 ~ "Mistreated",
                                 MISTREATC8 == 1 ~ "Mistreated",
                                 MISTREATC9 == 1 ~ "No mistreatment"), 
         CSECTFEEL_NEG = case_when(CSECTFEELC2 == 1 | CSECTFEELC4 == 1|
                                     CSECTFEELC6 == 1 | CSECTFEELC8 == 1 ~ 1, 
                                   TRUE ~ 0),
         CSECTFEEL_POS = case_when(CSECTFEELC1 == 1 | CSECTFEELC3 == 1|
                                     CSECTFEELC5 == 1 | CSECTFEELC7 == 1 ~ 1, 
                                   TRUE ~ 0)) 
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Others")
csfeel <- read.csv("CSECTFEEL_R.csv")
LTM2 <- LTM2 %>% 
  full_join(csfeel) %>%
  mutate(CSECTFEEL_POS = case_when(CSECTFEEL_POS == 1 | CSECTFEEL_POS_R == 1 ~ 1, 
                                   TRUE ~ 0),
         CSECTFEEL_NEG = case_when(CSECTFEEL_NEG == 1 | CSECTFEEL_NEG_R == 1 ~ 1, 
                                   TRUE ~ 0))


