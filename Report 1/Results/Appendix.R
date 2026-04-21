source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")

LTM_appendix <- LTM_final %>% 
  replace(is.na(.), 12345) %>% 
  mutate(FINALWT = as.numeric(FINALWT), 
         AGE_R = case_when(AGE >= 18 & AGE <= 19 ~ "18-19", 
                           AGE <= 24 ~ "20-24", 
                           AGE <= 29 ~ "25-29", 
                           AGE <= 34 ~ "30-34",
                           AGE >= 35 ~ "35+"), 
         PRENAT_TIME = case_when(LEARNED2 <= 12 ~ "1st trimester", 
                                 LEARNED2 <= 24 ~ "2nd trimester", 
                                 LEARNED2 == 1 ~  "3rd trimester or none", 
                                 LEARNED2 < 42 ~ "3rd trimester or none", 
                                 TRUE ~ "Missing"),
         PRETERM = case_when(GESTAGE < 37 ~ "Preterm", 
                             GESTAGE >= 37 & GESTAGE < 43 ~ "Term", 
                             TRUE ~ "Missing"))

LTM_appendix %>% 
  tbl_summary(include = c("AGE_R", "RACE", "BIRTHCOUNTRY","NUM_BIRTH1",
                          "EDUCAT","INSURCAT", "PRENAT_TIME",
                          "BIRTHATTEND",
                          "xMODE1", "VAGASSIST", "xMODE2",
                          "CSECTIONTYPE", "PRETERM",
                          "BW_CAT", "MEDINDUCE", "LABORINTC3", "EPIST"), 
              statistic = list(all_continuous() ~ "{mean}",
                               all_categorical() ~ "{p}"))

LTM_adsn <- LTM_appendix %>%
  as_survey_design(weight = FINALWT, id = 1)

LTM_adsn %>% 
  tbl_svysummary(include = c("AGE_R", "RACE", "BIRTHCOUNTRY","NUM_BIRTH1",
                             "EDUCAT","INSURCAT", "PRENAT_TIME",
                             "BIRTHATTEND",
                             "xMODE1", "VAGASSIST", "xMODE2",
                             "CSECTIONTYPE", "PRETERM",
                             "BW_CAT", "MEDINDUCE", "LABORINTC3", "EPIST"), 
                 statistic = list(all_continuous() ~ "{mean}",
                                  all_categorical() ~ "{p}"))


