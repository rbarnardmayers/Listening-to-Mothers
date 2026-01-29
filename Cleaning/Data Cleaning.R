source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")

# Data Read in &  Get rid of identifying information ----
LTM <- read.csv("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Data_FINAL.csv") 

LTM1 <- LTM %>% 
  mutate(MDID = as.character(MDID))

# Identify text response columns ending for other ----
# Don't want to convert these columns to numeric
ends_o <- LTM1[str_ends(colnames(LTM1), "O")]  %>% colnames()

# Create dataset of just text responses and MDID (so we can merge back) ----
# Don't want to convert these columns to numeric
LTM_ignore <- LTM1 %>% 
  select(c(all_of(ends_o), MDID, DOULA3, INDUCE6, REPEATCSEC,
           ResLanguage, ANYTHINGELSE, DISABILITYCOND)) 

# List of columns that are text only 
ignores <- LTM_ignore %>% 
  select(-c(MDID)) %>% 
  colnames() 

# Create dataset of the numeric columns to convert----
LTM_keep <- LTM1 %>% 
  select(-c(all_of(ignores)), MDID)

# Create list of column names to convert to numeric
tochange <- LTM_keep %>% select(-c(MDID)) %>%
  colnames()

# Convert all columns to numeric
for(i in tochange){
  LTM_keep[[i]] <- as.numeric(LTM_keep[[i]])
}

sn_cols <- LTM_keep %>% 
  select(starts_with("SN")) %>% 
  colnames()

for(i in sn_cols){
  LTM_keep[[i]] <- ifelse(LTM_keep[[i]] == 2, 0, 
                          ifelse(LTM_keep[[i]] == 99, NA, 
                                 ifelse(LTM_keep[[i]] == 99999, NA, 1)))
}
  
# merge the text columns back in with the numeric ones
LTM1 <- LTM_ignore %>% 
  full_join(LTM_keep, join_by(MDID))

# Recode variables ----
LTM2 <- LTM1 %>% 
  mutate(
    PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous",
                       NUMB_BIRTH > 1 ~ "Multiparous",
                       TRUE ~ "Missing"),
    DUEDATE_Y = as.numeric(DUEDATE_Y),
    DUEDATE_M = as.numeric(DUEDATE_M),
    DUEDATE_D = as.numeric(DUEDATE_D),
    DUEDATE = as.Date(paste0(DUEDATE_M,"/", DUEDATE_D, "/",DUEDATE_Y), "m%/%d/%Y"),
    
    YEARBIRTH = as.numeric(YEARBIRTH),
    BIRTHDATE_M = as.numeric(BIRTHDATE_M),
    BIRTHDATE_D = as.numeric(BIRTHDATE_D),
    BIRTHDATE = as.Date(paste0(BIRTHDATE_M,"/", BIRTHDATE_D, "/",YEARBIRTH), "m%/%d/%Y"),
    GESTAGE = as.numeric(difftime(BIRTHDATE, DUEDATE))/7 + 40,
    HEIGHT = (HEIGHT_FEET*12) + HEIGHT_INCHES, 
    PREPREG_WEIGHT = case_when(is.na(PREPREG_WEIGHT_A1) ~ 2.20462 * PREPREG_WEIGHT_B1, 
                               !is.na(PREPREG_WEIGHT_A1) ~ PREPREG_WEIGHT_A1), 
    PREG_WEIGHT = case_when(is.na(PREGWEIGHT_LBS_2) ~ 2.20462 * PREGWEIGHT_KG_2, 
                            !is.na(PREGWEIGHT_LBS_2) ~ PREGWEIGHT_LBS_2), 
    RACE = case_when(RACEALONE == 5 ~ "AIAN-NHPI",
                     RACEALONE == 7 ~ "AIAN-NHPI",
                     is.na(RACEALONE) ~ "Multi",
                     RACEALONE == 1 ~ "White",
                     RACEALONE == 2 ~ "Latine",
                     RACEALONE == 3 ~ "Black",
                     RACEALONE == 4 ~ "Asian",
                     RACEALONE == 6 ~ "MENA"),
    INSURANCE = case_when(INSURCAT == 1 ~ "Commercial",
                          INSURCAT == 2 ~ "Medicaid"),
    PROVIDER2 = case_when(PROVIDER %in% c(1,2,3)  ~ "Doctor",
                          PROVIDER == 4 ~ "Midwife",
                          PROVIDER %in% c(5,6) ~ "Other"),
    BMI = PREPREG_WEIGHT *703 / HEIGHT^2, 
    DOULA = case_when(DOULAC1 == 1 ~ "Yes", 
                      DOULAC2 == 1 ~ "Yes", 
                      DOULAC3 == 1 ~ "Yes",
                      DOULAC4 == 1 ~ "No", 
                      DOULAC5 == 1 ~ "Missing", 
                      TRUE ~ "Missing"), 
    DOULAC1_V2 = case_when(DOULA == "Yes" & DOULAC1 == 1 ~ "During Pregnancy", 
                        DOULA == "Yes" & DOULAC1 == 0 ~ "Not During Pregnancy", 
                        DOULA == "No" | DOULA == "Missing" ~ NA),
    DOULAC2_V2 = case_when(DOULA == "Yes" & DOULAC2 == 1 ~ "During Birth", 
                        DOULA == "Yes" & DOULAC2 == 0 ~ "Not During Birth", 
                        DOULA == "No" | DOULA == "Missing" ~ NA),
    DOULAC3_V2 = case_when(DOULA == "Yes" & DOULAC3 == 1 ~ "Postpartum", 
                        DOULA == "Yes" & DOULAC3 == 0 ~ "Not Postpartum", 
                        DOULA == "No" | DOULA == "Missing" ~ NA), 
    LEARNED1 = case_when(LEARNED1 == 99 ~ NA, 
                         is.na(LEARNED1) ~ NA,
                         TRUE ~ as.character(LEARNED1)), 
    PRENAT = case_when(LEARNED2 == 1 ~ "No Prenatal Care", 
                       LEARNED2 == 99 ~ "Missing", 
                       is.na(LEARNED2) ~ "Missing",
                       TRUE ~ "Had Prenatal Care"), 
    BIRTHWEIGHT_OZ = case_when(is.na(BIRTHWEIGHT_OZ) ~ 0, 
                               TRUE ~ BIRTHWEIGHT_OZ),
    BIRTHWEIGHT = case_when(!is.na(BIRTHWEIGHT_LBS) ~ BIRTHWEIGHT_LBS*453.592 + BIRTHWEIGHT_OZ*28.3495,
                            !is.na(BIRTHWEIGHT_G) ~ BIRTHWEIGHT_G), 
    BIRTHWEIGHT = as.numeric(BIRTHWEIGHT),
    BIRTHATTEND2 =case_when(BIRTHATTEND %in% c(1,2,3)  ~ "Doctor",
                            BIRTHATTEND == 4 ~ "Midwife",
                            BIRTHATTEND %in% c(5,6) ~ "Other"), 
    PPVISIT2 = case_when(PPVISIT >= 4 ~ 4,
                         TRUE ~ PPVISIT))


# rm(LTM1)
# Midwife, physician, other for provider

# Subscale respectful care ----
# LTM2$RESPECT <- likert('RESPECT')
# LTM2$KNOWLEDGE <- likert('KNOWLEDGE')
# LTM2$HEARD <- likert('HEARD')
# LTM2$DECISIONS <- likert('DECISIONS')
# LTM2$CONSENT <- likert('CONSENT')
# LTM2$INFORMED <- likert('INFORMED')
# LTM2$TIMELINESS <- likert('TIMELINESS')
# LTM2$TRUST <- likert('TRUST')
# LTM2$FEEDING <- likert('FEEDING')
# LTM2$SAFE <- likert('SAFE')
# 
# LTM2$DISCRIMINATION <- rev.likert('DISCRIMINATION')
# LTM2$NEGLECT <- rev.likert('NEGLECT')
# 
# LTM2$PPBOTHER_1 <- recode.phq("PPBOTHER_A1")
# LTM2$PPBOTHER_2 <- recode.phq("PPBOTHER_A2")
# LTM2$PPBOTHER_3 <- recode.phq("PPBOTHER_A3")
# LTM2$PPBOTHER_4 <- recode.phq("PPBOTHER_A4")
# 
# LTM2 <- LTM2 %>% 
#   mutate(PPBOTHER_1 = as.numeric(PPBOTHER_1), 
#          PPBOTHER_2 = as.numeric(PPBOTHER_2),
#          PPBOTHER_3 = as.numeric(PPBOTHER_3),
#          PPBOTHER_4 = as.numeric(PPBOTHER_4))
# LTM2$PHQ2 = LTM2$PPBOTHER_3 + LTM2$PPBOTHER_4
# LTM2$GAD2 = LTM2$PPBOTHER_1 + LTM2$PPBOTHER_2
# LTM2$PHQ4 = LTM2$PPBOTHER_1 + LTM2$PPBOTHER_2 + LTM2$PPBOTHER_3 + LTM2$PPBOTHER_4

LTM2 <- LTM2 %>% 
  mutate(#PHQ2_cat = case_when(PHQ2 < 3 ~ "No", 
    #                      PHQ2 >= 3 ~ "Yes"), 
    # GAD2_cat = case_when(GAD2 < 3 ~ "No", 
    #                      GAD2 >= 3 ~ "Yes"), 
    # PHQ4_cat = case_when(PHQ4 >= 0 & PHQ4 <= 2 ~ "None",
    #                      PHQ4 > 2 & PHQ4 <= 5 ~ "Mild",
    #                      PHQ4 > 5 & PHQ4 <= 8 ~ "Moderate", 
    #                      PHQ4 > 8 ~ "Severe"),
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
    # MODE15 = case_when(MODE15 == 1 ~ 0, 
    #                    MODE15 == 2 ~ 1),
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
  rename(MDE2023 = MODE2023, 
         SONEEDC11 = SOCIALNEEDC11,
         SONEEDC10 = SOCIALNEEDC10, 
         HSPFEEDC8 = HOSPFEEDC8,
         HSPFEEDC11 = HOSPFEEDC11,
         HSPFEEDC12 = HOSPFEEDC12) %>%
  rowwise() %>% 
  mutate(MODE_ALL = sum(across(starts_with("MODE")), na.rm = T),
         SUM_SOCIALNEED =sum(across(starts_with("SOCIALNEED")), na.rm = T),
         SUM_HOSPFEED = sum(across(starts_with("HOSPFEED")), na.rm = T),
         SUM_RESPECT = sum(RESPECT, KNOWLEDGE, HEARD, 
                           DECISIONS, CONSENT, INFORMED,
                           TIMELINESS, TRUST, FEEDING,
                           SAFE, DISCRIMINATION, NEGLECT, na.rm = T),
         SUM_SNNEEDS = sum(across(starts_with("SN")), na.rm = T),
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
  rename(MODE2023 = MDE2023,
         SOCIALNEEDC11 = SONEEDC11,
         SOCIALNEEDC10 = SONEEDC10, 
         HOSPFEEDC8 = HSPFEEDC8,
         HOSPFEEDC11 = HSPFEEDC11,
         HOSPFEEDC12 = HSPFEEDC12)

for(i in sn_cols){
  LTM2[[i]] <- ifelse(LTM2[[i]] == 0, 2, LTM2[[i]])
}


# Recoding missing values ----
missing_d <- dict2 %>% 
  select(c("missing", "variable")) %>% 
  mutate(missing = as.numeric(missing)) %>% 
  subset(!is.na(missing)) %>%
  mutate(KEEP = case_when(startsWith(variable, "x") ~ 0, TRUE ~ 1)) %>% 
  subset(KEEP == 1)

missing_99 <- missing_d %>% subset(missing == 99)
missing_999 <- missing_d %>% subset(missing == 999)
missing_99999 <- missing_d %>% subset(missing == 99999)
missing_999999999999 <- missing_d %>% subset(missing == 999999999999)

LTM_99 <- LTM2 %>% 
  select(c(missing_99$variable, MDID))  %>% 
  replace_with_na_all(condition = ~.x ==  99) 

LTM_999 <- LTM2 %>% 
  select(c(missing_999$variable, MDID)) %>% 
  replace_with_na_all(condition = ~.x ==  999) 

LTM_99999 <- LTM2 %>% 
  select(c(missing_99999$variable, MDID)) %>% 
  replace_with_na_all(condition = ~.x ==  99999) 

LTM_999999999999 <- LTM2 %>% 
  select(c(missing_999999999999$variable, MDID)) %>% 
  replace_with_na_all(condition = ~.x ==  999999999999) 

LTM3 <- LTM2 %>% 
  select(-c(missing_d$variable)) %>%
  full_join(LTM_99, join_by(MDID)) %>% 
  full_join(LTM_999, join_by(MDID)) %>% 
  full_join(LTM_99999, join_by(MDID)) %>% 
  full_join(LTM_999999999999, join_by(MDID))%>% 
  mutate(SUM_HOSPFEED = case_when(SUM_HOSPFEED > 10 ~ NA, 
                                  TRUE ~ SUM_HOSPFEED),
         SUM_SNNEEDS = case_when(SUM_SNNEEDS > 10 ~ NA, 
                                  TRUE ~ SUM_SNNEEDS), 
         CARETYPE_R = case_when(CARETYPEC1 == 1  & CARETYPEC2 == 0 ~ 1, 
                                CARETYPEC1 == 1 & CARETYPEC2 == 1 ~ 2, 
                                CARETYPEC1 == 0 & CARETYPEC2 == 1 ~ 3), 
         CAREMODE_R = case_when(CAREMODEC1 == 1  & CAREMODEC2 == 0 ~ 1, 
                                CAREMODEC1 == 1 & CAREMODEC2 == 1 ~ 2, 
                                CAREMODEC1 == 0 & CAREMODEC2 == 1 ~ 3))

# Exporting ----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers")
write.csv(LTM3, "LTM_clean.csv")
