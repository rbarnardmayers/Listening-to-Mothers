source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 1/Cleaning/ApplyDictionary.R")

LTM_appendix <- LTM_final %>% 
  replace(is.na(.), 12345) %>% 
  mutate(FINALWT = as.numeric(FINALWT), 
         AGE_R = case_when(AGE >= 18 & AGE <= 24 ~ "18-24", 
                           AGE <= 29 ~ "25-29", 
                           AGE <= 34 ~ "30-34",
                           AGE >= 35 ~ "35+"), 
         PRENAT_TIME = case_when(LEARNED2 <= 12 ~ "1st trimester", 
                                 LEARNED2 <= 24 ~ "2nd trimester", 
                                 LEARNED2 == 1 ~  "3rd trimester or none", 
                                 LEARNED2 < 42 ~ "3rd trimester or none"),
         PRETERM = case_when(GESTAGE < 37 ~ "Preterm", 
                             GESTAGE >= 37 & GESTAGE < 43 ~ "Term", 
                             TRUE ~ NA),
         RACE_R = case_when(RACEC1 == "White" & RACEC2 == "Hispanic or Latina" ~ "Hispanic Latina",
                            RACEALONE == "Latine ALONE" ~ "Hispanic Latina",
                            RACEALONE == 12345 ~ "Multiracial",
                            TRUE ~ RACEALONE),
         BIRTHCOUNTRY = case_when(BIRTHCOUNTRY == "I’d prefer not to answer" ~ NA, 
                                  TRUE  ~ BIRTHCOUNTRY),
         EDUCAT = case_when(EDUCAT == 12345 ~ NA, 
                            TRUE ~ EDUCAT),
         PRENAT_TIME = case_when(PRENAT_TIME == "Missing" ~ NA,
                                 TRUE  ~ PRENAT_TIME),
         BIRTHATTEND = case_when(BIRTHATTEND =="I’m not sure" ~ NA, 
                                 BIRTHATTEND == "I’d prefer not to answer" ~ NA,
                               TRUE  ~ BIRTHATTEND),
         BW_CAT = case_when(BW_CAT == 12345 ~ NA, 
                            TRUE ~ BW_CAT),
         VAGASSIST = case_when(VAGASSIST == "Forceps to help the baby out" ~ "Assisted", 
                               VAGASSIST == "Vacuum cup to help the baby out" ~ "Assisted", 
                               VAGASSIST == "Neither" ~ "Unassissted"),
         INSURANCE = case_when(INSURCAT == "Commercial" ~ "Private", 
                               INSURCAT == "Medicaid/CHIP" ~ "Medicaid", 
                               INSURCAT == "No insurance" ~ "None", 
                               is.na(INSURCAT) ~ NA,
                               INSURCAT == 12345 ~ NA,
                               TRUE ~ "Other")
         
  )

LTM_appendix %>% 
  tbl_summary(include = c("AGE_R", "RACE_R", "BIRTHCOUNTRY","NUM_BIRTH1",
                          "EDUCAT","INSURANCE", "PRENAT_TIME",
                          "BIRTHATTEND",
                          "xMODE1", "VAGASSIST", "xMODE2",
                          "CSECTIONTYPE", "PRETERM",
                          "BW_CAT", "MEDINDUCE", "LABORINTC3", "EPIST"), 
              statistic = list(all_continuous() ~ "{mean}",
                               all_categorical() ~ "{p}"),
              digits = list(BW_CAT = 3))

LTM_adsn <- LTM_appendix %>%
  as_survey_design(weight = FINALWT, id = 1)

LTM_adsn %>% 
  tbl_svysummary(include = c("AGE_R", "RACE_R", "BIRTHCOUNTRY","NUM_BIRTH1",
                             "EDUCAT","INSURANCE", "PRENAT_TIME",
                             "BIRTHATTEND",
                             "xMODE1", "VAGASSIST", "xMODE2",
                             "CSECTIONTYPE", "PRETERM",
                             "BW_CAT", "MEDINDUCE", "LABORINTC3", "EPIST"), 
                 statistic = list(all_continuous() ~ "{mean}",
                                  all_categorical() ~ "{p}"),
                 digits = list(BIRTHATTEND = 3,
                               xMODE2 = 3))

# Insurance problem
prop.table(table(LTM_appendix$INSURCAT))

LTM_appendix %>% 
  tbl_summary(by = INSURCAT, 
              include = c(INSURC1, INSURC2, INSURC3, INSURC4))

LTM_appendix %>% 
  filter(INSURCAT == "Commercial") %>%
  tbl_summary(by = INSURC4, 
              include = c(INSURC1, INSURC2, INSURC3))





