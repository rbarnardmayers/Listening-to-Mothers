# Algorithm Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")

# Data Read in &  Get rid of identifying information ----
LTM <- read.csv("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Data_TEMP.csv")
LTM <- LTM %>% 
  mutate(RACE = case_when(RACEC1 == 1 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NHW", 
                          RACEC1 == 0 & RACEC2 == 1 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "Hispanic", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 1 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Black or African American", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 1 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Asian", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 1 & RACEC6 == 0 & RACEC7 == 0 ~ "AIAN", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 1 & RACEC7 == 0 ~ "MENA", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 1 ~ "NHPI", 
                          RACEC1 == 1 | RACEC2 == 1 | RACEC3 == 1 | RACEC4 == 1 | RACEC5 == 1 | RACEC6 == 1 | RACEC7 == 1 ~ "Multiracial", 
                          RACEC8 == 1 ~ "Prefer not to answer", 
                          TRUE ~ "Missing"),
         PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous", 
                            NUMB_BIRTH > 1 ~ "Multiparous", 
                            TRUE ~ "Missing"), 
         INCOME = as.numeric(INCOME),
         PREPREG_WEIGHT_A1 = as.numeric(PREPREG_WEIGHT_A1),
         PREPREG_WEIGHT_B1 = as.numeric(PREPREG_WEIGHT_B1),
         PREGWEIGHT_A1 = as.numeric(PREGWEIGHT_A1),
         PREGWEIGHT_B1 = as.numeric(PREGWEIGHT_B1),
         PREPREG_WEIGHT = case_when(is.na(PREPREG_WEIGHT_A1) ~ 2.20462 * PREPREG_WEIGHT_B1, 
                                    !is.na(PREPREG_WEIGHT_A1) ~ PREPREG_WEIGHT_A1), 
         PREG_WEIGHT = case_when(is.na(PREGWEIGHT_A1) ~ 2.20462 * PREGWEIGHT_B1,
                                 !is.na(PREGWEIGHT_A1) ~ PREGWEIGHT_A1),
         PREPREG_WEIGHT = round(PREPREG_WEIGHT, 5),
         PREG_WEIGHT = round(PREG_WEIGHT, 5),
         BIRTHWEIGHT_LBS = as.numeric(BIRTHWEIGHT_LBS),
         BIRTHWEIGHT_G = as.numeric(BIRTHWEIGHT_G),
         BIRTHWEIGHT_OZ = as.numeric(BIRTHWEIGHT_OZ),
         BIRTHWEIGHT_OZ = case_when(is.na(BIRTHWEIGHT_OZ) ~ 0, 
                                    TRUE ~ BIRTHWEIGHT_OZ),
         CURRWEIGHT_KG = as.numeric(CURRWEIGHT_KG),
         HEIGHT = (as.numeric(HEIGHT_FEET)*12) + as.numeric(HEIGHT_INCHES),
         BIRTHWEIGHT = case_when(!is.na(BIRTHWEIGHT_LBS) ~ BIRTHWEIGHT_LBS*453.59237 + BIRTHWEIGHT_OZ*28.3495,
                                 !is.na(BIRTHWEIGHT_G) ~ BIRTHWEIGHT_G), 
         BIRTHWEIGHT = as.numeric(BIRTHWEIGHT),
         GESTAGE_WEEKS = as.numeric(GESTAGE_WEEKS),
         KG_any = case_when(!is.na(CURRWEIGHT_KG)  &  CURRWEIGHT_KG != 0~ 1, 
                            !is.na(PREGWEIGHT_B1) &  PREGWEIGHT_B1 != 0 ~ 1, 
                            !is.na(PREPREG_WEIGHT_B1) & PREPREG_WEIGHT_B1 != 0 ~ 1, 
                            TRUE ~ 0), 
         LEARNED1 = case_when(LEARNED1 == 99 ~ NA, TRUE ~ LEARNED1), 
         LABORINTC5 = as.numeric(LABORINTC5),
         LABCSEC = as.numeric(LABCSEC),
         CSECTIONTYPE = as.numeric(CSECTIONTYPE),
         VAGASSIST= as.numeric(VAGASSIST),
         CARESETTINGC1 = as.numeric(CARESETTINGC1),
         CARESETTINGC2 = as.numeric(CARESETTINGC2),
         CARESETTINGC3 = as.numeric(CARESETTINGC3),
         CARESETTINGC4 = as.numeric(CARESETTINGC4),
         CARESETTINGC5 = as.numeric(CARESETTINGC5),
         CARESETTINGC6 = as.numeric(CARESETTINGC6),
         CARESETTINGC7 = as.numeric(CARESETTINGC7),
         
         CS_MIDWIFE = case_when(BIRTHATTEND == 4 & MODE2023 == 2 ~ 1, 
                                BIRTHATTEND == 5 & MODE2023 == 2 ~ 1,
                                TRUE ~ 0),
         PROVIDER = as.numeric(PROVIDER),
         PPVISITTIME1=as.numeric(PPVISITTIME1),
         PPVISITTIME2=as.numeric(PPVISITTIME2),
         
         PPVISITTIME = case_when(!is.na(PPVISITTIME1) ~ PPVISITTIME1,
                                 !is.na(PPVISITTIME2) ~ PPVISITTIME2),
         
         # Flag 1s
         F1_DUP = DUP_FLAG,
         F1_SPEED = case_when(TotalDurationSec < 19*60 ~ 1, 
                              TotalDurationSec >= 19*60 ~ 0),
         F1_USKG_ANY = case_when(BIRTHCOUNTRY == 1 & KG_any == 1 ~ 1, 
                                 TRUE ~ 0),
         F1_NOTRIBALAFF = case_when(RACE == "AIAN" & "99" %in% AIAN  ~ 1,
                                    RACE == "AIAN" & AIAN == "99 " ~ 1,
                                    RACE == "AIAN" & AIAN == " 99" ~ 1,
                                    RACE == "AIAN" & AIAN == "11" ~ 1,
                                    RACE == "AIAN" & AIAN == "Na" ~ 1,
                                    RACE == "AIAN" & AIAN == "None" ~ 1,
                                    RACE == "AIAN" & AIAN == "N/A" ~ 1,
                                    RACE == "AIAN" & AIAN == "None Officially" ~ 1,
                                    RACE == "AIAN" & AIAN == "Don't Have One" ~ 1,
                                    RACE == "AIAN" & AIAN == "American Indian /African American/white" ~ 1,
                                    RACE == "AIAN" & AIAN == " " ~ 1,
                                    RACE == "AIAN" & AIAN == "African American" ~ 1,
                                    RACE == "AIAN" & AIAN == "" ~ 1,
                                    RACE == "AIAN" & AIAN == "American" ~ 1,
                                    RACE == "AIAN" & AIAN == "American Indian" ~ 1,
                                    TRUE ~ 0),
         
         # Flag 2s
         F2_GA_EXT = case_when(GESTAGE_WEEKS < 27 ~ 1, 
                               GESTAGE_WEEKS > 42 ~ 1, 
                               TRUE ~ 0),
         F2_HT_EXT = case_when(HEIGHT <= 57 ~ 1, 
                               HEIGHT >= 77 ~ 1, 
                               TRUE ~ 0),
         F2_WT_EXT = case_when(PREG_WEIGHT <= 115 ~ 1, 
                               PREG_WEIGHT >= 340 ~ 1, 
                               PREPREG_WEIGHT <= 94 ~ 1, 
                               PREPREG_WEIGHT >= 317 ~ 1, 
                               TRUE ~ 0),
         F2_BW_EXT = case_when(BIRTHWEIGHT <= 970 ~ 1, 
                               BIRTHWEIGHT >= 4620 & PREPREG_PHYSCONDC2 == 0 & 
                                 PREGCONDITIONC1 == 0 ~ 1,
                               BIRTHWEIGHT >= 4800 & PREPREG_PHYSCONDC2 == 1 ~ 1,
                               BIRTHWEIGHT >= 4800 & PREGCONDITIONC1 == 1 ~ 1,
                               TRUE ~ 0),
         F2_MCAID_INC_EXT = case_when(INCOME > 150000 & INSURC2 == 1 ~ 1, 
                                      TRUE ~ 0), 
         F2_VLBNICU = case_when(BIRTHWEIGHT < 1500 & NICU == 3 ~ 1, 
                                TRUE ~ 0), 
         F2_VLBHOSP = case_when(BIRTHWEIGHT < 1500 & BABYHOSP< 4 ~ 1, 
                                TRUE ~ 0), 
         
         # Flag 3s
         F3_PNCB4LEARN = case_when(as.numeric(LEARNED2) < as.numeric(LEARNED1) ~ 1, 
                                   TRUE ~ 0),
         F3_EARLYPNC = case_when(LEARNED2 > 1 & LEARNED2 <= 4 ~ 1, 
                                 TRUE ~ 0),
         F3_NO_PNC = case_when(LEARNED2 == 1 ~ 1, 
                               TRUE ~ 0),
         F3_PREGWT = case_when(PREG_WEIGHT <= 129 & F2_WT_EXT == 0 ~ 1, 
                               TRUE ~ 0),
         F3_WTDIFF = case_when(PREG_WEIGHT < PREPREG_WEIGHT ~ 1, 
                               TRUE ~ 0 ),
         F3_HT = case_when(HEIGHT >= 70 & HEIGHT < 77 ~ 1, 
                           TRUE ~ 0),
         F3_LBW_NICU = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                   NICU == 3 ~ 1, 
                                 TRUE ~ 0),
         F3_LBW_DAYS = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                   BABYHOSP < 4 ~ 1, 
                                 TRUE ~ 0),
         
         F3_VAGASSIST = case_when(VAGASSIST %in% c(1,2) ~ 1, 
                                  TRUE ~ 0),
         F3_GAFOOD = case_when(CURRENTFEEDC4 == 0 & BABY_SURVEYAGE_MONTHS >= 7  ~ 1, 
                               TRUE ~ 0),
         F3_INC_MCAID = case_when(INCOME < 150000 & INCOME >= 100000 & 
                                    INSURC2 == 1 ~ 1, 
                                  TRUE ~ 0),
         F3_INC_PAN = case_when(INCOME >= 150000 & Source %in% c(4,5,6,7) ~ 1, 
                                TRUE ~ 0),
         NEED_F = case_when(SOCIALNEEDC1 == 1 ~ 1, 
                            SOCIALNEEDC2 == 1 ~ 1,
                            SOCIALNEEDC3 == 1 ~ 1,
                            SOCIALNEEDC4 == 1 ~ 1,
                            TRUE ~ 0),
         F3_INC_NEEDS = case_when(INCOME >= 150000 & NEED_F == 1 ~ 1,
                                  TRUE ~ 0),
         F3_PRE_HYPER = case_when(PREPREG_PHYSCONDC1 == 1 ~ 1, 
                                  TRUE ~ 0),
         F3_PRE_DIABETES = case_when(PREPREG_PHYSCONDC2 == 1 ~ 1,
                                     TRUE ~ 0),
         F3_CS_LABOR = case_when(LABCSEC == 1 & CSECTIONTYPE == 1 ~ 1, 
                                 TRUE ~ 0),
         
         F3_VAGEXAM = case_when(LABORINTC5 == 0 ~ 1, 
                                TRUE ~ 0),
         F3_TERM_NICU = case_when(GESTAGE_WEEKS >= 37 & NICU %in% c(1,2) ~ 1, 
                                  TRUE ~ 0),
         F3_PPTVISITS = case_when(PPVISIT == 99 ~ 1, 
                                  TRUE ~ 0),
         F3_MISSING_BW = case_when(is.na(BIRTHWEIGHT) ~ 1, 
                                   TRUE ~ 0),
         F3_MISSING_LABORHRS = case_when(is.na(as.numeric(LABORLENGTH)) ~ 1, 
                                         TRUE ~ 0),
         F3_MISSING_MOMDAYS = case_when(is.na(DAYSHOSP)~ 1, 
                                        DAYSHOSP == 99 ~ 1,
                                        TRUE ~ 0),
         F3_MISSING_BABYDAYS = case_when(is.na(BABYHOSP) ~ 1, 
                                         BABYHOSP == 99 ~ 1,
                                         TRUE ~ 0)) %>% 
  rowwise() %>%
  mutate(FLAG1 = sum(across(c(F1_SPEED, F1_USKG_ANY, F1_DUP))),
         FLAG2 = sum(across(starts_with("F2_"))),
         FLAG3 = sum(across(starts_with("F3_"))), 
         FLAGSUM = sum(across(c("FLAG2", "FLAG3")))) %>% 
  ungroup()

# setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM")
# write.csv(LTM, "LTM_RUBY_FLAGREVISION.csv")

# Full data	
# Deletes removed	
#3 strikes and out 
# < 5 total flags 	< 4 total flags	< 6 total flags	< 6 total flags	< 5* total flags	< 5* total flags	< 4* total flags	< 4* total flags	National Data

# ALGORITHM PROCESS ----
LTM <- LTM %>% 
  rename(F4_PRE_HYPER = F3_PRE_HYPER, 
         F4_PRE_DIABETES = F3_PRE_DIABETES, 
         F4_NO_PNC = F3_NO_PNC, 
         F4_CS_LABOR = F3_CS_LABOR, 
         F4_VAGEXAM = F3_VAGEXAM,
         F4_TERM_NICU = F3_TERM_NICU, 
         F4_PPTVISITS = F3_PPTVISITS, 
         F4_MISSING_BABYDAYS = F3_MISSING_BABYDAYS,
         F4_MISSING_BW = F3_MISSING_BW, 
         F4_MISSING_LABORHRS = F3_MISSING_LABORHRS, 
         F4_MISSING_MOMDAYS = F3_MISSING_MOMDAYS) %>% 
  rowwise() %>%
  mutate() %>% 
  mutate(FLAG1 = sum(across(c(F1_SPEED, F1_USKG_ANY, F1_DUP))),
         FLAG2 = sum(across(starts_with("F2_"))),
         FLAG3 = sum(across(starts_with("F3_"))), 
         FLAG4 = sum(across(starts_with("F4_"))),
         FLAG23 = sum(across(c("FLAG2", "FLAG3"))), 
         FLAG234 = sum(across(c("FLAG2", "FLAG3", "FLAG4")))) %>% 
  ungroup()

# Full Dataset 
t_0 <- LTM %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER == 1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  t() %>% as.data.frame()
rownames(t_0) <- c("n", "CESAREAN","MARRIED","NULLIPAROUS","SOLIDFOOD" ,
                   "VAGASSIST","EARLYPNC","HYPERTENSION",
                   "DIABETES", "NOPNC", "OBGYN", "CS_MIDWIFE")
colnames(t_0) <- c("Full Dataset")

# First Step 
t_1 <- LTM %>% 
  group_by(FLAG1) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG1 == 0) %>%
  t() %>% as.data.frame()
t_1 <- t_1[-1,] %>% as.data.frame()
rownames(t_1) <- c("n","CESAREAN","MARRIED","NULLIPAROUS","SOLIDFOOD" ,
                   "VAGASSIST","EARLYPNC","HYPERTENSION",
                   "DIABETES", "NOPNC", "OBGYN", "CS_MIDWIFE")
colnames(t_1) <- c("Deletes removed")

# Second Step 
t_2 <- LTM %>% 
  subset(FLAG1 == 0) %>% 
  mutate(FLAG2 = case_when(FLAG2 < 3 ~ 0, 
                           FLAG2 >= 3 ~ 1)) %>%
  group_by(FLAG2) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG2 == 0) %>%
  t() %>% as.data.frame()
t_2 <- t_2[-1,] %>% as.data.frame()
rownames(t_2) <- c("n","CESAREAN","MARRIED","NULLIPAROUS","SOLIDFOOD" ,
                   "VAGASSIST","EARLYPNC","HYPERTENSION",
                   "DIABETES", "NOPNC", "OBGYN", "CS_MIDWIFE")
colnames(t_2) <- c("Three Strikes and Out")

# Third Step (< 4)

t_3_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3) %>% 
  mutate(FLAG23 = case_when(FLAG23 < 4 ~ 0, 
                            FLAG23 >= 4 ~ 1)) %>%
  group_by(FLAG23) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG23 == 0) %>%
  t() %>% as.data.frame()
t_3_4 <- t_3_4[-1,] %>% as.data.frame()
rownames(t_3_4) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                     "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                     "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_3_4) <- c("Step 3a < 4 Total Flags")

# Third Step (< 5)
t_3_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3) %>% 
  mutate(FLAG23 = case_when(FLAG23 < 5 ~ 0, 
                            FLAG23 >= 5 ~ 1)) %>%
  group_by(FLAG23) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG23 == 0) %>%
  t() %>% as.data.frame()
t_3_5 <- t_3_5[-1,] %>% as.data.frame()
rownames(t_3_5) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                     "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                     "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_3_5) <- c("Step 3b < 5 Total Flags")

# Fourth Step (< 4 & < 4)
t_4_4_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 4 ~ 0, 
                             FLAG234 >= 4 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_4 <- t_4_4_4[-1,] %>% as.data.frame()
rownames(t_4_4_4) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_4_4) <- c("Step 4aa < 4 Total Flags")

# Fourth Step (< 4 & < 5)
t_4_4_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 5 ~ 0, 
                             FLAG234 >= 5 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_5 <- t_4_4_5[-1,] %>% as.data.frame()
rownames(t_4_4_5) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_4_5) <- c("Step 4ab < 5 Total Flags")

# Fourth Step (< 4 & < 6)
t_4_4_6 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 6 ~ 0, 
                             FLAG234 >= 6 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_6 <- t_4_4_6[-1,] %>% as.data.frame()
rownames(t_4_4_6) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_4_6) <- c("Step 4ac < 6 Total Flags")

# Fourth Step (< 5 & < 4)
t_4_5_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 4 ~ 0, 
                             FLAG234 >= 4 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_4 <- t_4_5_4[-1,] %>% as.data.frame()
rownames(t_4_5_4) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_5_4) <- c("Step 4ba < 4 Total Flags")

# Fourth Step (< 5 & < 5)
t_4_5_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 5 ~ 0, 
                             FLAG234 >= 5 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_5 <- t_4_5_5[-1,] %>% as.data.frame()
rownames(t_4_5_5) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_5_5) <- c("Step 4bb < 5 Total Flags")

# Fourth Step (< 5 & < 6)
t_4_5_6 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 6 ~ 0, 
                             FLAG234 >= 6 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(),
            CESAREAN=round(sum(MODE2023 == 2)/n, 5),
            MARRIED=round(sum(RELATIONSHIP == 1)/n, 5),
            NULLIPAROUS=round(sum(PARITY == "Nulliparous")/n, 5),
            SOLIDFOOD=round(sum(F3_GAFOOD == 1)/n, 5),
            VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 5),
            EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 5),
            HYPERTENSION=round(sum(F4_PRE_HYPER == 1)/n, 5),
            DIABETES=round(sum(F4_PRE_DIABETES == 1)/n, 5),
            NOPNC=round(sum(LEARNED2 == 1)/n, 5),
            OBGYN = round(sum(PROVIDER ==1, na.rm = T)/n, 5), 
            CS_MIDWIFE = round(sum(CS_MIDWIFE == 1)/n,5)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_6 <- t_4_5_6[-1,] %>% as.data.frame()
rownames(t_4_5_6) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                       "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                       "F3_PRE_DIABETES", "NO_PNC", "OBGYN", "CS_MIDWIFE")
colnames(t_4_5_6) <- c("Step 4bc < 6 Total Flags")

# MERGING TOGETHER ----
all_results <- cbind(t_0, t_1, t_2, t_3_5, t_3_4, 
                     t_4_5_6,t_4_4_6,t_4_5_5, 
                     t_4_4_5, t_4_5_4, t_4_4_4)
all_results$NatDat <- c(NA, .32, .53, .40, .08, .03, .04, .03, .01, .02, NA, NA)
all_results$measure <- rownames(all_results)
all_results <- all_results %>% relocate(measure)

all_results %>% View()


