source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")

# Data Read in &  Get rid of identifying information ----
LTM <- read.csv("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Data_FINAL.csv") 

LTM1 <- LTM %>% 
  mutate(MDID = as.character(MDID)) %>% 
  select(-c(ResEmail, ResPhone, DialingMode,ManualDialing,resTimeZone,
            ResPIN,SurveyName,ResLanguage, PanelistId, CallBackDate,
            LastConnectionDate,LastConnectionWeek,LastConnectionStartTime,
            ConnectionDurationInSeconds,ConnectionDurationInMinutes,
            LastQuestionFilled,NumberOfConnections,resDisposition,
            ResCaseResult,TotalDurationSec,Device,DeviceOS,DeviceVersion,
            DeviceBrowser,DeviceBrowserVersion,GeoLocation,OfflineUser,
            AppointmentDate,SurveyLink,AccessExpiration,CustomResult,
            AgentId,AgentUserName,Priority,CallNote,ResCompleted,
            ResBlocked,IsAnonymized,ResActive,Modified,DNC,Callback,
            PIN,RID,PID,PID1,REFID1,NAME,ADDRESS,CITY,ZIPCODE,BATCH,
            CHILDNAME, contains("FLAG"),starts_with("SCREEN"),
            starts_with("F1"), starts_with("F2"), starts_with("F3"), 
            starts_with("x"), starts_with("X")))


# Identify text response columns ending for other ----
ends_o <- LTM1[str_ends(colnames(LTM1), "O")] %>% 
  select(-c(INTRO)) %>% colnames()

# Create dataset of just text responses and MDID ----
LTM_ignore <- LTM1 %>% 
  select(c(all_of(ends_o), MDID, DOULA3, INDUCE6, REPEATCSEC,
           MEDINDUCE4, MEDINDUCE5, WENTWELL, DIDNTGOWELL, ANYTHINGELSE, AIAN,
           DISABILITYCOND, Source)) 

ignores <- LTM_ignore %>% 
  select(-c(MDID)) %>% 
  colnames() 

# Create dataset of the rest of the columns ----
LTM_keep <- LTM1 %>% 
  select(-c(all_of(ignores)), MDID, ST)

# Create list of column names to conver to numeric
tochange <- LTM_keep %>% 
  select(-c(MDID)) %>% 
  colnames()

# Convert to numeric
for(i in tochange){
  LTM_keep[[i]] <- as.numeric(LTM_keep[[i]])
}

# for(i in colnames(LTM_ignore)){
#   if(i %in% colnames(LTM_keep)){
#     print(i)
#   } 
# }

LTM1 <- LTM_ignore %>% 
  full_join(LTM_keep, join_by(MDID))

# test2 <- LTM_keep[131,] %>% select(c(UID2))
# test1 <- LTM_ignore[1,] %>% select(c(UID2))
# 
# LTM_ignore[LTM_ignore$UID2 == test1$UID2, ] %>% 
#   relocate(UID2) %>% 
#   View()
# 
# LTM_keep[LTM_keep$UID2 == test2$UID2, ] %>% 
#   relocate(UID2) %>% 
#   View()

rm(LTM_ignore)
rm(LTM_keep)

# Recode variables ----

LTM2 <- LTM1 %>% 
  mutate(PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous", 
                            NUMB_BIRTH > 1 ~ "Multiparous", 
                            TRUE ~ "Missing"),
         # DUEDATE_Y = as.numeric(DUEDATE_Y),
         # DUEDATE_M = as.numeric(DUEDATE_M),
         # DUEDATE_D = as.numeric(DUEDATE_D),
         # DUEDATE = as.Date(paste0(DUEDATE_M,"/", DUEDATE_D, "/",DUEDATE_Y), "m%/%d/%Y"),
         # 
         # YEARBIRTH = as.numeric(YEARBIRTH),
         # BIRTHDATE_M = as.numeric(BIRTHDATE_M),
         # BIRTHDATE_D = as.numeric(BIRTHDATE_D),
         # BIRTHDATE = as.Date(paste0(BIRTHDATE_M,"/", BIRTHDATE_D, "/",YEARBIRTH), "m%/%d/%Y"),
         # GESTAGE = as.numeric(difftime(BIRTHDATE, DUEDATE))/7 + 40,
         HEIGHT = (HEIGHT_FEET*12) + HEIGHT_INCHES, 
         PREPREG_WEIGHT = case_when(is.na(PREPREG_WEIGHT_A1) ~ 2.20462 * PREPREG_WEIGHT_B1, 
                                    !is.na(PREPREG_WEIGHT_A1) ~ PREPREG_WEIGHT_A1), 
         PREG_WEIGHT = case_when(is.na(PREGWEIGHT_LBS_2) ~ 2.20462 * PREGWEIGHT_KG_2, 
                                 !is.na(PREGWEIGHT_LBS_2) ~ PREGWEIGHT_LBS_2), 
         RACE = case_when(RACEC1 == 1 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NHW", 
                          RACEC1 == 0 & RACEC2 == 1 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "Hispanic", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 1 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Black or African American", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 1 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Asian", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 1 & RACEC6 == 0 & RACEC7 == 0 ~ "AIAN", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 1 & RACEC7 == 0 ~ "MENA", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 1 ~ "NHPI", 
                          RACEC1 == 1 | RACEC2 == 1 | RACEC3 == 1 | RACEC4 == 1 | RACEC5 == 1 | RACEC6 == 1 | RACEC7 == 1 ~ "Multiracial", 
                          RACEC8 == 1 ~ "Prefer not to answer", 
                          TRUE ~ "Missing"),
         INSURANCE = case_when(INSURC1 == 1 ~ "Private",
                               INSURC2 == 1 ~ "Medicaid/CHIP",
                               INSURC3 == 1 ~ "Other",#"TRICARE or other military health care",
                               INSURC4 == 1 ~ "Other",#"Indian Health Service or tribal",
                               INSURC5 == 1 ~ "Other",#"Other",
                               INSURC6 == 1 ~ "None",#"None",
                               INSURC7 == 1 ~ "Missing",
                               TRUE ~ "Missing"),
         LANGUAGE = case_when(LANGHOMEC1 == 1 ~ "English",
                              LANGHOMEC1 == 0 ~ "Other",
                              TRUE ~ "Missing"),
         BMI = PREPREG_WEIGHT *703 / HEIGHT^2, 
         DOULA = case_when(DOULAC1 == 1 ~ "Yes", 
                           DOULAC2 == 1 ~ "Yes", 
                           DOULAC3 == 1 ~ "Yes",
                           DOULAC4 == 1 ~ "No", 
                           DOULAC5 == 1 ~ "Missing", 
                           TRUE ~ "Missing"), 
         DOULAC1 = case_when(DOULA == "Yes" & DOULAC1 == 1 ~ "During Pregnancy", 
                             DOULA == "Yes" & DOULAC1 == 0 ~ "Not During Pregnancy", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC2 = case_when(DOULA == "Yes" & DOULAC2 == 1 ~ "During Birth", 
                             DOULA == "Yes" & DOULAC2 == 0 ~ "Not During Birth", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC3 = case_when(DOULA == "Yes" & DOULAC3 == 1 ~ "Postpartum", 
                             DOULA == "Yes" & DOULAC3 == 0 ~ "Not Postpartum", 
                             DOULA == "No" | DOULA == "Missing" ~ NA), 
         LEARNED1 = case_when(LEARNED1 == 99 ~ "Missing", 
                              is.na(LEARNED1) ~ "Missing",
                              TRUE ~ as.character(LEARNED1)), 
         PRENAT = case_when(LEARNED2 == 1 ~ "No Prenatal Care", 
                            LEARNED2 == 99 ~ "Missing", 
                            is.na(LEARNED2) ~ "Missing",
                            TRUE ~ "Had Prenatal Care"), 
         BIRTHWEIGHT_OZ = case_when(is.na(BIRTHWEIGHT_OZ) ~ 0, 
                                    TRUE ~ BIRTHWEIGHT_OZ),
         BIRTHWEIGHT = case_when(!is.na(BIRTHWEIGHT_LBS) ~ BIRTHWEIGHT_LBS*453.592 + BIRTHWEIGHT_OZ*28.3495,
                                 !is.na(BIRTHWEIGHT_G) ~ BIRTHWEIGHT_G), 
         BIRTHWEIGHT = as.numeric(BIRTHWEIGHT))


rm(LTM1)
# Midwife, physician, other for provider

# Subscale respectful care ----
LTM2$RESPECT <- likert('RESPECT')
LTM2$KNOWLEDGE <- likert('KNOWLEDGE')
LTM2$HEARD <- likert('HEARD')
LTM2$DECISIONS <- likert('DECISIONS')
LTM2$CONSENT <- likert('CONSENT')
LTM2$INFORMED <- likert('INFORMED')
LTM2$TIMELINESS <- likert('TIMELINESS')
LTM2$TRUST <- likert('TRUST')
LTM2$FEEDING <- likert('FEEDING')
LTM2$SAFE <- likert('SAFE')

LTM2$DISCRIMINATION <- rev.likert('DISCRIMINATION')
LTM2$NEGLECT <- rev.likert('NEGLECT')


LTM2$PPBOTHER_1 <- recode.phq("PPBOTHER_A1")
LTM2$PPBOTHER_2 <- recode.phq("PPBOTHER_A2")
LTM2$PPBOTHER_3 <- recode.phq("PPBOTHER_A3")
LTM2$PPBOTHER_4 <- recode.phq("PPBOTHER_A4")

LTM2 <- LTM2 %>% 
  mutate(PPBOTHER_1 = as.numeric(PPBOTHER_1), 
         PPBOTHER_2 = as.numeric(PPBOTHER_2),
         PPBOTHER_3 = as.numeric(PPBOTHER_3),
         PPBOTHER_4 = as.numeric(PPBOTHER_4))
LTM2$PHQ2 = LTM2$PPBOTHER_3 + LTM2$PPBOTHER_4
LTM2$GAD2 = LTM2$PPBOTHER_1 + LTM2$PPBOTHER_2
LTM2$PHQ4 = LTM2$PPBOTHER_1 + LTM2$PPBOTHER_2 + LTM2$PPBOTHER_3 + LTM2$PPBOTHER_4

LTM2 <- LTM2 %>% 
  mutate(PHQ2_cat = case_when(PHQ2 < 3 ~ "No", 
                              PHQ2 >= 3 ~ "Yes"), 
         GAD2_cat = case_when(GAD2 < 3 ~ "No", 
                              GAD2 >= 3 ~ "Yes"), 
         PHQ4_cat = case_when(PHQ4 >= 0 & PHQ4 <= 2 ~ "None",
                              PHQ4 > 2 & PHQ4 <= 5 ~ "Mild",
                              PHQ4 > 5 & PHQ4 <= 8 ~ "Moderate", 
                              PHQ4 > 8 ~ "Severe"),
         FEED_CONCORDANT = case_when(PLANNEDFEEDC1 == 1 & FEED1WEEKC1 == 1 ~ 1, 
                                     PLANNEDFEEDC2 == 1 & FEED1WEEKC2 == 1 ~ 1, 
                                     PLANNEDFEEDC1 == 1 &  FEED1WEEKC2 == 1 ~ 0,
                                     PLANNEDFEEDC2 == 1 &  FEED1WEEKC1 == 1 ~ 0),
         MODE = case_when(MODE == 1 ~ 0, 
                          MODE == 2 ~ 1),
         # MODE1 = case_when(MODE1 == 1 ~ 0, 
         #                   MODE1 == 2 ~ 1),
         MODE2 = case_when(MODE2 == 1 ~ 0, 
                           MODE2 == 2 ~ 1),
         MODE3 = case_when(MODE3 == 1 ~ 0, 
                           MODE3 == 2 ~ 1),
         MODE4 = case_when(MODE4 == 1 ~ 0, 
                           MODE4 == 2 ~ 1),
         MODE5 = case_when(MODE5 == 1 ~ 0, 
                           MODE5 == 2 ~ 1),
         MODE6 = case_when(MODE6 == 1 ~ 0, 
                           MODE6 == 2 ~ 1),
         MODE7 = case_when(MODE7 == 1 ~ 0, 
                           MODE7 == 2 ~ 1),
         MODE8 = case_when(MODE8 == 1 ~ 0, 
                           MODE8 == 2 ~ 1),
         MODE9 = case_when(MODE9 == 1 ~ 0, 
                           MODE9 == 2 ~ 1),
         MODE10 = case_when(MODE10 == 1 ~ 0, 
                            MODE10 == 2 ~ 1),
         MODE11 = case_when(MODE11 == 1 ~ 0, 
                            MODE11 == 2 ~ 1),
         MODE12 = case_when(MODE12 == 1 ~ 0, 
                            MODE12 == 2 ~ 1),
         MODE13 = case_when(MODE13 == 1 ~ 0, 
                            MODE13 == 2 ~ 1),
         MODE14 = case_when(MODE14 == 1 ~ 0, 
                            MODE14 == 2 ~ 1),
         MODE15 = case_when(MODE15 == 1 ~ 0, 
                            MODE15 == 2 ~ 1),
         phys_cb = case_when(PAINMEDSC1 == 1 ~ "Physiologic Childbirth", 
                             PAINMEDSC2 == 1 ~ "Physiologic Childbirth", 
                             PAINMEDSC3 == 1 ~ "Physiologic Childbirth", 
                             PAINMEDSC4 == 1 ~ "Physiologic Childbirth", 
                             PAINMEDSC5 == 1 ~ "Physiologic Childbirth", 
                             EPIST == 1 ~ "Physiologic Childbirth", 
                             TRUE ~ "Non-Physiologic Childbirth"),
         SDM_1 = case_when(INDUCE1 == 1 ~ 1, 
                           INDUCE1 == 2 ~ 1, 
                           INDUCE1 == 3 ~ 0, 
                           INDUCE1 == 4 ~ 0),
         SDM_2 = case_when(INDUCE2 == 1 ~ 1, 
                           INDUCE2 == 2 ~ 1, 
                           INDUCE2 == 3 ~ 0, 
                           INDUCE2 == 4 ~ 0),
         SDM_3 = case_when(INDUCE3 == 1 ~ 1, 
                           INDUCE3 == 2 ~ 0),
         SDM_4 = case_when(INDUCE4 == 1 ~ 1, 
                           INDUCE4 == 2 ~ 0),
         WEIGHTGAIN = PREG_WEIGHT - PREPREG_WEIGHT,
         BIRTHWEIGHT_CAT = case_when(BIRTHWEIGHT < 1500 ~ "VLBW",
                                     BIRTHWEIGHT < 2500 ~ "LBW",
                                     BIRTHWEIGHT > 4000 ~ "LBW",
                                     is.na(BIRTHWEIGHT) ~ NA,
                                     TRUE ~ "Normal BW"),
         BPCONFID = case_when(BPCONFID == 99 ~ NA,
                              TRUE ~ BPCONFID), 
         URINECONFID = case_when(URINECONFID == 99 ~ NA,
                                 TRUE ~ URINECONFID),
         WEIGHCONFID = case_when(WEIGHCONFID == 99 ~ NA,
                                 TRUE ~ WEIGHCONFID),
         BABYHRCONFID = case_when(BABYHRCONFID == 99 ~ NA,
                                  TRUE ~ BABYHRCONFID), 
         CONFIDENCE_ANY = case_when(BPCONFID < 3  | URINECONFID < 3 | 
                                      WEIGHCONFID < 3 | BABYHRCONFID < 3 ~ "Yes",
                                    TRUE ~ "No"), 
         ATHOMECARE_ANY = case_when( ATHOMECAREC1 == 1 ~ 1, 
                                     ATHOMECAREC2 == 1 ~ 1, 
                                     ATHOMECAREC3 == 1 ~ 1,
                                     ATHOMECAREC4 == 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  rename(MDE2023 = MODE2023) %>%
  rowwise() %>% 
  mutate(MODE_ALL = sum(across(starts_with("MODE")), na.rm = T),
         SUM_RESPECT = sum(RESPECT, KNOWLEDGE, HEARD, 
                           DECISIONS, CONSENT, INFORMED,
                           TIMELINESS, TRUST, FEEDING,
                           SAFE, DISCRIMINATION, NEGLECT, na.rm = T),
         SDM = sum(SDM_1,SDM_2,SDM_3,SDM_4, na.rm = T)) 

LTM2 <- LTM2 %>% 
  mutate(VBAC = case_when(MODE_ALL > 0 & MDE2023 == 1 ~ 1,
                          MODE_ALL > 0 & MDE2023 == 2 ~ 0,
                          MODE_ALL == 0 ~ 0),
         SDM = case_when(is.na(SDM_1)~NA,
                         is.na(SDM_2)~NA,
                         is.na(SDM_3)~NA,
                         is.na(SDM_4)~NA, 
                         TRUE ~ SDM), 
         MDID = as.numeric(MDID)) %>%
  rename(MODE2023 = MDE2023)

setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers")
write.csv(LTM2, "LTM_clean.csv")
