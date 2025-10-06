# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Data Cleaning.R")

# Data dictionary prep ----
# Getting rid of columns with no conversion in dict

LTM_include <- LTM2 %>% 
  select(-c(NUMB_BIRTH, HEIGHT_FEET, HEIGHT_INCHES, LEARNED2,LEARNED1,
            DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,  VAGEXAM, LABORLENGTH, DAYSHOSP,
            BABYHOSP, PPVISIT, PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,PARITY, 
            HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, DOULA, 
            DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT, WEAN, ZIP, FAMSIZE1, 
            FAMSIZE2, INCOME, IMMIGRATION, all_of(ignores)))

# 
LTM2 <- LTM2 %>% 
  select(c(NUMB_BIRTH, HEIGHT_FEET, HEIGHT_INCHES, LEARNED2,LEARNED1,
           DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,  VAGEXAM, LABORLENGTH, DAYSHOSP,
           BABYHOSP, PPVISIT, PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,
           PARITY, HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, 
           DOULA, DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT,
           WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION, UID2, 
           all_of(ignores)))

# Apply dictionary recoding and labeling 
LTM_include <- recode_vrs(data = LTM_include, data_dictionary = data_dict, 
                          vrs = colnames(LTM_include))
# recode NA to missing
LTM_include[is.na(LTM_include)] <- "Missing"

# Final dataset 
LTM_final <- LTM2 %>% 
  full_join(LTM_include)

# Create a weight variable while waiting for final version of dataset ----
LTM_final <- LTM_final %>% 
  mutate(wght = rnorm(nrow(LTM_final), mean = 1, sd = .02))

# Create list of categorical variables by excluding numeric ----
categorical <- LTM_final %>%
  select(-c(HEIGHT,CaseId,MDID,ST, INTRO, AGE,SCREEN,TIMES,YEARBIRTH, 
            all_of(ignores),BW_LBSANDOZ,SURVEYYEAR,SURVEYMONTHS,SURVEYDAYS,
            SURVEYDATE,
            ends_with("BIRTHYEAR"), starts_with("SCREEN"), starts_with("TRAP"),
            CURRWEIGHT_LBS,PREGWEIGHT_B1,PREGWEIGHT_A1,
            starts_with("FOLLOWUP"),
            starts_with("x"),contains("FLAG"), starts_with("F2_"),ends_with("O"),
            starts_with("F3_"),  starts_with("F1_"),starts_with("X"),
            SCREEN3,NUMB_BIRTH_OLD, PREG_INT,PREPREG_WEIGHT_B1,LEARNED2,
            TRAP_AREA_SELECTOR,NUMB_BIRTH,HEIGHT_FEET,HEIGHT_INCHES,CURRWEIGHT_KG,
            LABORPERMIT_D1,LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,
            BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGEBIRTH,AGECHECK,
            DISABLEYRS,DKREF_COUNT,DKREF_PERCENT,
            INTERNATIONAL_IP,AGEDIFF, ATTENTION_CHECK,BIRTHDATE,wght,
            DUEDATE,DAYSDIFFERENCE,GESTAGE,GESTAGE_WEEKS,LABORPERMIT_D1,
            LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,PREG_INT,
            GESTAGE_DAYS,HEIGHTFT_IN,COMBINEDHEIGHT,PREGWEIGHTCHECK, 
            PREPREG_WEIGHT_A1,PREGCONDITIONC11, PREPREGWEIGHTCHECK,
            DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,LEARNED1,
            VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, PREG_WEIGHT,
            PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,ZIPSTATE,
            HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, DOULA,
            DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT, PRENAT,IMMIGRATION,
            WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION, "RANDOM",
            "SURMODE","META_USERAGENT"  ,"META_REFERER" , "META_IPADDRESS" ,
            "META_BROWSER_LANGUAGE","META_JAVASUPPORT","META_OPERATINGSYSTEM",UID )) %>%
  select(-c(UID2)) %>%
  colnames()

# Factor categorical variables and reorder values----
for(i in categorical){
  LTM_final[[i]] <- refac.fun(i)
}

# Catgorical by Chapter ----

# cat_2 <- LTM_final %>% 
#   select(c("NUMB_BIRTH", 'PARITY', 'PREG_INT', starts_with("PREPREG_PH"), 
#            starts_with("PREPREG_MH"), 'PROVIDER', 'PROVIDERCHOICE', 
#            DOULA, DOULA1, LEARNED1, LEARNED2, starts_with("CARESETTING"), 
#            starts_with("CARETYPE"), starts_with("CAREMODE"), )) %>%
#   colnames()
# cont_2 <- LTM_final %>% 
#   select(c(PREG_WEIGHT, HEIGHT, BMI, PREPREG_WEIGHT)) %>%
#   colnames()
# 
# cat_3 <- c()
# cat_4 <- c()
# cat_5 <- c()
# cat_6 <- c()


# Convert continuous variables into numeric class ----
continuous <- LTM_final %>%  
  select(c(AGE,YEARBIRTH,BABY_SURVEYAGE_MONTHS,
           NUMB_BIRTH,BIRTHWEIGHT,
           BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGEBIRTH,
           DISABLEYRS,PREG_WEIGHT, GESTAGE,
           FIRSTVISIT, VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, 
           PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,
           HEIGHT, PREPREG_WEIGHT, BMI,
           WEAN, FAMSIZE1, FAMSIZE2, INCOME )) %>% 
  colnames()

for(i in continuous){
  LTM_final[[i]] <- as.numeric(LTM_final[[i]])
}

rm(LTM_include)
rm(LTM2)
gc()

# Subscale respectful care ----
LTM_final$RESPECT <- likert('RESPECT')
LTM_final$KNOWLEDGE <- likert('KNOWLEDGE')
LTM_final$HEARD <- likert('HEARD')
LTM_final$DECISIONS <- likert('DECISIONS')
LTM_final$CONSENT <- likert('CONSENT')
LTM_final$INFORMED <- likert('INFORMED')
LTM_final$TIMELINESS <- likert('TIMELINESS')
LTM_final$TRUST <- likert('TRUST')
LTM_final$FEEDING <- likert('FEEDING')
LTM_final$SAFE <- likert('SAFE')

LTM_final$DISCRIMINATION <- rev.likert('DISCRIMINATION')
LTM_final$NEGLECT <- rev.likert('NEGLECT')


LTM_final <- LTM_final %>% 
  mutate(FEED_CONCORDANT = case_when(PLANNEDFEEDC1 == 1 & FEED1WEEKC1 == 1 ~ 1, 
                                     PLANNEDFEEDC2 == 1 & FEED1WEEKC2 == 1 ~ 1, 
                                     PLANNEDFEEDC1 == 1 &  FEED1WEEKC2 == 1 ~ 0,
                                     PLANNEDFEEDC2 == 1 &  FEED1WEEKC1 == 1 ~ 0),
         MODE = case_when(MODE == 1 ~ 0, 
                          MODE == 2 ~ 1),
         MODE1 = case_when(MODE1 == 1 ~ 0, 
                           MODE1 == 2 ~ 1),
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
                            MODE15 == 2 ~ 1)) %>% 
  rename(MDE2023 = MODE2023)
rowwise() %>% 
  mutate(MODE_ALL = sum(across(starts_with("MODE")), na.rm = T)) 

mutate(VBAC = case_when(MODE_ALL > 0 & MDE2023 == 1 ~ 1,
                        MODE_ALL > 0 & MDE2023 == 2 ~ 0,
                        MODE_ALL == 0 ~ 0))

# Respect care: developer’s 0-100 scoring guidance
rowwise() %>% 
  mutate(SUM_RESPECT = sum(RESPECT, KNOWLEDGE, HEARD, 
                           DECISIONS, CONSENT, INFORMED,
                           TIMELINESS, TRUST, FEEDING,
                           SAFE, DISCRIMINATION, NEGLECT, na.rm = T)) %>% 
  mutate(phys_cb = case_when(PAINMEDSC1 == 1 ~ 1, 
                             PAINMEDSC2 == 1 ~ 1, 
                             PAINMEDSC3 == 1 ~ 1, 
                             PAINMEDSC4 == 1 ~ 1, 
                             PAINMEDSC5 == 1 ~ 1, 
                             EPIST == 1 ~ 1, 
                             TRUE ~ 0),
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
                           INDUCE4 == 2 ~ 0)) %>% 
  rowwise() %>%
  mutate(SDM = sum(SDM_1,SDM_2,SDM_3,SDM_4, na.rm = T)) %>%
  mutate(SDM = case_when(is.na(SDM_1)~NA,
                         is.na(SDM_2)~NA,
                         is.na(SDM_3)~NA,
                         is.na(SDM_4)~NA, 
                         TRUE ~ SDM),
         PREG_WEIGHT - PREPREG_WEIGHT,
         BIRTHWEIGHT_CAT = case_when(BIRTHWEIGHT < 1500 ~ "VLBW",
                                     BIRTHWEIGHT < 2500 ~ "LBW",
                                     BIRTHWEIGHT > 4,000 ~ "LBW",
                                     is.na(BRITHWEIGHT) ~ NA,
                                     TRUE ~ "Normal BW"),
         BPCONFID = case_when(BPCONFID == 99 ~ NA), 
         URINECONFID = case_when(URINECONFID == 99 ~ NA),
         WEIGHCONFID = case_when(WEIGHCONFID == 99 ~ NA),
         BABYHRCONFID = case_when(BABYHRCONFID == 99 ~ NA), 
         
         CONFIDENCE_ANY = case_when(BPCONFID < 3  | URINECONFID < 3 | 
                                      WEIGHCONFID < 3 | BABYHRCONFID < 3 ~ "Yes",
                                    TRUE ~ "No"), 
         ATHOMECARE_ANY = case_when( ATHOMECAREC1 == 1 ~ 1, 
                                     ATHOMECAREC2 == 1 ~ 1, 
                                     ATHOMECAREC3 == 1 ~ 1,
                                     ATHOMECAREC4 == 1 ~ 1, 
                                     TRUE ~ 0))

LTM_dsn <- LTM_final %>% 
  as_survey_design(weight = wght, id = 1)
