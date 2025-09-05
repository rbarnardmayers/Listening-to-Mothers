source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Helpful_Functions.R")

# Data Read in ----
LTM <- read.csv("Data_7.25.25.csv")

# Get rid of identifying information 
LTM1 <- LTM[,c(1,2,46,47,55,74:736)]

LTM1 <- LTM1 %>% 
  select(-c(CHILDNAME)) 

# Convert numeric cols ----
births <- LTM1 %>% select(ends_with("BIRTHYEAR")) %>% names()
pren <- LTM1 %>% select(starts_with("NOPRENATALC")) %>% 
  select(-c("NOPRENATALC13O")) %>% names()
cares <- LTM1 %>% select(c(starts_with("CARESETTING"), starts_with("CARETYPE"), 
                           starts_with("CAREMODE"), starts_with("WHYTELEC"), 
                           starts_with("ATHOMECAREC"))) %>% 
  select(-c("CARESETTINGC8O", "WHYTELEC7O")) %>%  names()
educ <- LTM1 %>% select(starts_with("EDUCMODE"), starts_with("EDUCIMPACT")) %>% 
  select(-c('EDUCIMPACTC7O')) %>% names()
empl <- LTM1 %>% select(starts_with("EMPLOY")) %>% names()
douls <- LTM1 %>% select(starts_with("DOULA")) %>% 
  select(-c('DOULA1C6O', 'DOULA2C6O', 'DOULA3')) %>% names()
induc <- LTM1 %>% select(starts_with("INDUCE"), 
                         starts_with("SELFINDUCE"), starts_with("MEDINDUCE"), 
                         starts_with("MODE")) %>% 
  select(-c('INDUCE6', "SELFINDUCE2C8O","MEDINDUCE3C8O")) %>% names()
drugs <- LTM1 %>%select(c(starts_with("DRUGFREE"))) %>% 
  select(-c(DRUGFREEC10O)) %>% names()
fetal <- LTM1 %>%select(c(starts_with("FETALMON"))) %>% 
  select(-c(FETALMONC3O)) %>% names()
csec <- LTM1 %>%select(c(starts_with("CSECTIONINT"), starts_with("LABORINT"), 
                         starts_with("LABORPERMIT"))) %>% 
  select(-c('LABORPERMIT_D1', 'LABORPERMIT_D2')) %>% names()
post <- LTM1 %>%select(c(starts_with("POSITION2"))) %>% 
  select(-c("POSITION2C7O")) %>% names()
disc <- LTM1 %>%select(c(starts_with("DISCRIMINATION"))) %>% 
  select(-c(ends_with("O"))) %>% names()
topic <- LTM1 %>%select(c(starts_with("VISITTOPIC"))) %>% 
  select(-c(ends_with("O"))) %>% names()
hospf <- LTM1 %>%select(c(starts_with("HOSPFEED"))) %>% 
  select(-c(ends_with("O"))) %>% names()
sn <- LTM1 %>%select(c(starts_with("SN"))) %>% 
  select(-c(ends_with("O"))) %>% names()

allnames <- c(births, pren, cares, educ, empl, douls, induc, drugs, 
              fetal, csec, post, disc, topic, hospf, sn)

LTM2 <-  convert.fun(LTM1, c('MDID', allnames, 'PREPREG_WEIGHT_B1', 
                             'NUMB_BIRTH_OLD', 
                             'PREPREG_WEIGHT_A1', 'FIRSTVISIT', 'PROVIDER', 
                             'PROVIDERCHOICE', 'BPCONFID', 'URINECONFID', 'WEIGHCONFID', 
                             'BABYHRCONFID', 'CARETYPEPREF', 'CAREMODEPREF', 'PRIOREDUC', 
                             'CURREDUC', 'TRAP1', 'BIGBABY2','PREGWEIGHT_B1', 
                             'CURRWEIGHT_KG', 'CURRWEIGHT_LBS', 'CSECTIONTYPE', 
                             'LABCSEC', 'PLANNEDC','VBACCHOICE', 'VBACINTEREST', 
                             'VBACEFFORT', 'VBACACCESSC1', 'VBACACCESSC2', 
                             'VBACACCESSC3', 'VBACACCESSC4', 'VBACACCESSC5', 
                             'UNPLANNEDREASON', 'VAGASSIST', 'VAGEXAM', 'LABORWALK', 
                             'EPIST', 'EPISTCHOICE', 'POSITION', 'POSITIONCHOICE', 
                             'LABORLENGTH', 'BIRTHWEIGHT_LBS', 'BIRTHWEIGHT_OZ', 
                             'BIRTHWEIGHT_G', 'SUTURE', 'SKIN', 'TRAP2', 'VISITS', 
                             'PPVISIT', 
                             'PPVISITTIME1', 'PPVISITTIME2', 'EXCLUSIVEBF', 
                             'EXCLUSIVEBF', 'EXCLBFGOAL', 'WEAN', 'BFGOAL', 'TRAP3'))

# Convert to factors ----
LTM3 <- convert.yn(LTM2, c('PREGCONDITIONC1', 'PREGCONDITIONC2', 'PREGCONDITIONC3', 'PREGCONDITIONC4', 
                           'PREPREG_MHCONDC1', 'PREPREG_MHCONDC2', 'PREPREG_MHCONDC3', 'PREPREG_MHCONDC4',
                           'PREPREG_MHCONDC5', 'PREPREG_MHCONDC6', "DOULAC1", "DOULAC2", "DOULAC3", 
                           'DOULAC4', 'DOULAC5'))

# Recode variables ----
LTM_final <- LTM3 %>% 
  mutate(wght = rnorm(1613, mean = 1, sd = .02), 
         PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous", 
                            NUMB_BIRTH > 1 ~ "Multiparous", 
                            TRUE ~ "Missing"),
         HEIGHT = (HEIGHT_FEET*12) + HEIGHT_INCHES, 
         PREPREG_WEIGHT = case_when(is.na(PREPREG_WEIGHT_A1) ~ 2.20462 * PREPREG_WEIGHT_B1, 
                                    !is.na(PREPREG_WEIGHT_A1) ~ PREPREG_WEIGHT_A1), 
         RACE = case_when(RACEC2 == 1 ~ "Hispanic", 
                          RACEC1 == 1 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NHW", 
                          RACEC1 == 0 & RACEC3 == 1 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Black or African American", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 1 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Asian", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 1 & RACEC6 == 0 & RACEC7 == 0 ~ "AIAN", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 1 & RACEC7 == 0 ~ "MENA", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 1 ~ "NHPI", 
                          RACEC1 == 1 | RACEC3 == 1 | RACEC4 == 1 | RACEC5 == 1 | RACEC6 == 1 | RACEC7 == 1 ~ "Multiracial", 
                          RACEC8 == 1 ~ "Missing", 
                          TRUE ~ "Missing"),
         PREG_INT = case_when(PREG_INT == 1 ~ "Yes, but I was hoping to become pregnant sooner",
                              PREG_INT == 2 ~ "Yes, I wanted to become pregnant at that time",
                              PREG_INT == 3 ~ "Yes, but I was hoping to be pregnant later on",
                              PREG_INT == 4 ~ "No, I didn’t want to be pregnant then or at any time in the future",
                              PREG_INT == 99 ~ "Missing", 
                              is.na(PREG_INT) ~ "Missing"),
         PROVIDER = case_when(PROVIDER == 1 ~ "Obstetrician-gynecologist doctor",
                              PROVIDER == 2 ~ "Family medicine doctor",
                              PROVIDER == 3 ~ "Midwife",
                              PROVIDER == 4 ~ "Nurse-practitioner/other nurse",
                              PROVIDER == 5 ~ "Physician assistant",
                              PROVIDER == 6 ~ "Missing",
                              PROVIDER == 99 ~ "Missing",
                              is.na(PROVIDER) ~ "Missing"),
         PROVIDER = factor(PROVIDER, levels = c("Obstetrician-gynecologist doctor", 
                                                "Nurse-practitioner/other nurse",
                                                "Physician assistant",
                                                "Family medicine doctor",
                                                "Midwife", "Missing")),
         PROVIDERCHOICE = case_when(PROVIDERCHOICE == 1 ~ "Yes, I had a choice and saw one person",
                                    PROVIDERCHOICE == 2 ~ "Yes, I had a choice and generally saw members of a small team",
                                    PROVIDERCHOICE == 3 ~ "No I had no choice",
                                    PROVIDERCHOICE == 99 ~ "Missing", 
                                    is.na(PROVIDERCHOICE) ~ "Missing"),
         INSURANCE = case_when(INSURC1 == 1 ~ "Private", 
                               INSURC2 == 1 ~ "Medicaid/CHIP", 
                               INSURC3 == 1 ~ "TRICARE or other military health care", 
                               INSURC4 == 1 ~ "Indian Health Service or tribal", 
                               INSURC5 == 1 ~ "Other", 
                               INSURC6 == 1 ~ "None", 
                               INSURC7 == 1 ~ "Missing", 
                               TRUE ~ "Missing"),
         LANGUAGE = case_when(LANGHOMEC1 == 1 ~ "English",
                              LANGHOMEC1 == 0 ~ "Other",
                              TRUE ~ "Missing"),
         BIRTHCOUNTRY = case_when(BIRTHCOUNTRY == 1 ~ "US", 
                                  BIRTHCOUNTRY == 2 ~ "Outside US", 
                                  BIRTHCOUNTRY == 99 ~ "Missing",
                                  is.na(BIRTHCOUNTRY) ~ "Missing"),
         DISABLETECH = case_when(DISABLETECH == 1 ~ "Yes", 
                                 DISABLETECH == 2 ~ "No", 
                                 DISABLETECH == 99 ~ "Missing", 
                                 is.na(DISABLETECH) ~ "Missing"),
         BMI = PREPREG_WEIGHT *703 / HEIGHT^2, 
         DOULA = case_when(DOULAC1 == 'Yes' ~ "Yes", 
                           DOULAC2 == 'Yes' ~ "Yes", 
                           DOULAC3 == 'Yes' ~ "Yes",
                           DOULAC4 == 'Yes' ~ "No", 
                           DOULAC5 == 'Yes' ~ "Missing", 
                           TRUE ~ "Missing"), 
         DOULAC1 = case_when(DOULA == "Yes" & DOULAC1 == "Yes" ~ "During Pregnancy", 
                             DOULA == "Yes" & DOULAC1 == "No" ~ "Not During Pregnancy", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC2 = case_when(DOULA == "Yes" & DOULAC2 == "Yes" ~ "During Birth", 
                             DOULA == "Yes" & DOULAC2 == "No" ~ "Not During Birth", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC3 = case_when(DOULA == "Yes" & DOULAC3 == "Yes" ~ "Postpartum", 
                             DOULA == "Yes" & DOULAC3 == "No" ~ "Not Postpartum", 
                             DOULA == "No" | DOULA == "Missing" ~ NA), 
         LEARNED2 = case_when(LEARNED2 == 1 ~ "No Prenatal Care", 
                              LEARNED2 == 99 ~ "Missing", 
                              is.na(LEARNED2) ~ "Missing", 
                              TRUE ~ as.character(LEARNED2)),
         LEARNED1 = case_when(LEARNED1 == 99 ~ "Missing", 
                              TRUE ~ as.character(LEARNED1)), 
         PRENAT = case_when(LEARNED2 == "No Prenatal Care" ~ "No Prenatal Care", 
                            LEARNED2 == "Missing" ~ "Missing", 
                            TRUE ~ "Had Prenatal Care"), 
         FIRSTVISIT = case_when(FIRSTVISIT ==1 ~ "Yes", 
                                FIRSTVISIT == 2 ~"No", 
                                FIRSTVISIT == 99 ~ "Missing", 
                                is.na(FIRSTVISIT) ~ "Missing"))

# Refactor ----
col_list <- c('PROVIDER', 'PROVIDERCHOICE', 'BIRTHCOUNTRY', 'PARITY', 
              'DISABLETECH', 'DOULA','LANGUAGE', 'INSURANCE', 'PREG_INT', 'RACE',
              "PRENAT", "FIRSTVISIT")
for(i in col_list){
  
  LTM_final[[i]] <- refac.fun(i)
  
}

LTM_final$LEARNED1 <- refac.num('LEARNED1')
LTM_final$LEARNED2 <- refac.num("LEARNED2", val = c("No Prenatal Care", "Missing"))
  


# Remove previous versions ----
rm(LTM1)
rm(LTM2)
rm(LTM)

