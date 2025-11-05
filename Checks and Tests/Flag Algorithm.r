# Algorithm Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
library(simplecolors)
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
         gest_age_h = case_when(as.numeric(GESTAGE_WEEKS) > 42 ~ 1, 
                                TRUE ~ 0), 
         gest_age_l = case_when(as.numeric(GESTAGE_WEEKS) < 27 ~ 1, 
                                TRUE ~ 0),
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
         BIRTHWEIGHT_LBSONLY = case_when(!is.na(BIRTHWEIGHT_LBS) ~ BIRTHWEIGHT_LBS + BIRTHWEIGHT_OZ * 0.0625,
                                         !is.na(BIRTHWEIGHT_G) ~ BIRTHWEIGHT_G*0.00220462),
         BIRTHWEIGHT = as.numeric(BIRTHWEIGHT),
         GESTAGE_WEEKS = as.numeric(GESTAGE_WEEKS),
         KG_any = case_when(!is.na(CURRWEIGHT_KG)  &  CURRWEIGHT_KG != 0~ 1, 
                            !is.na(PREGWEIGHT_B1) &  PREGWEIGHT_B1 != 0 ~ 1, 
                            !is.na(PREPREG_WEIGHT_B1) & PREPREG_WEIGHT_B1 != 0 ~ 1, 
                            TRUE ~ 0), 
         LEARNED1 = case_when(LEARNED1 == 99 ~ NA, TRUE ~ LEARNED1), 
         
         # Flag 1s
         # SPEED = case_when(ConnectionDurationInMinutes < 19 ~ 1, 
         #                   ConnectionDurationInMinutes >= 19 ~ 0),
         SPEED = case_when(TotalDurationSec < 19*60 ~ 1, 
                           TotalDurationSec >= 19*60 ~ 0),
         USKGANY = case_when(BIRTHCOUNTRY == 1 & KG_any == 1 ~ 1, 
                             TRUE ~ 0),
         NOTRIBALAFF = case_when(RACE == "AIAN" & AIAN == "99" ~ 1,
                                 RACE == "AIAN" & AIAN == "99 " ~ 1,
                                 RACE == "AIAN" & AIAN == " 99" ~ 1,
                                 RACE == "AIAN" & AIAN == "11" ~ 1,
                                 RACE == "AIAN" & AIAN == "Na" ~ 1,
                                 RACE == "AIAN" & AIAN == "None" ~ 1,
                                 RACE == "AIAN" & AIAN == "N/A" ~ 1,
                                 RACE == "AIAN" & AIAN == "Don't Have One" ~ 1,
                                 RACE == "AIAN" & AIAN == "American Indian /African American/white" ~ 1,
                                 RACE == "AIAN" & AIAN == " " ~ 1,
                                 RACE == "AIAN" & AIAN == "African American" ~ 1,
                                 RACE == "AIAN" & AIAN == "" ~ 1,
                                 TRUE ~ 0),
         
         # Flag 2s
         GA_EXT = case_when(GESTAGE_WEEKS < 27 ~ 1, 
                            GESTAGE_WEEKS > 42 ~ 1, 
                            TRUE ~ 0),
         HT_EXT = case_when(HEIGHT <= 57 ~ 1, 
                            HEIGHT >= 77 ~ 1, 
                            TRUE ~ 0),
         WT_EXT = case_when(PREG_WEIGHT <= 115 ~ 1, 
                            PREG_WEIGHT >= 340 ~ 1, 
                            PREPREG_WEIGHT <= 94 ~ 1, 
                            PREPREG_WEIGHT >= 317 ~ 1, 
                            TRUE ~ 0),
         BW_EXT = case_when(BIRTHWEIGHT <= 970 ~ 1, 
                            BIRTHWEIGHT >= 4620 & PREPREG_PHYSCONDC2 == 1 ~ 1,
                            BIRTHWEIGHT >= 4620 & PREGCONDITIONC1 == 1 ~ 1,
                            BIRTHWEIGHT >= 4800 & PREPREG_PHYSCONDC2 == 0 ~ 1,
                            BIRTHWEIGHT >= 4800 & PREGCONDITIONC1 == 0 ~ 1,
                            TRUE ~ 0),
         MCAID_INC_EXT = case_when(INCOME >= 150000 & INSURC2 == 1 ~ 1, 
                                   TRUE ~ 0), 
         VLBNICU = case_when(BIRTHWEIGHT < 1500 & NICU == 3 ~ 1, 
                             TRUE ~ 0), 
         VLBHOSP = case_when(BIRTHWEIGHT < 1500 & BABYHOSP< 4 ~ 1, 
                             TRUE ~ 0), 
         
         # Flag 3s
         PNCB4LEARN = case_when(as.numeric(LEARNED2) < as.numeric(LEARNED1) ~ 1, 
                                TRUE ~ 0),
         EARLYPNC = case_when(LEARNED2 > 1 & LEARNED2 <= 4 ~ 1, 
                              TRUE ~ 0),
         NO_PNC = case_when(LEARNED2 == 1 ~ 1, 
                            TRUE ~ 0),
         PREGWT = case_when(PREG_WEIGHT <= 129 & WT_EXT == 0 ~ 1, 
                            TRUE ~ 0),
         WTDIFF = case_when(PREG_WEIGHT < PREPREG_WEIGHT ~ 1, 
                            TRUE ~ 0 ),
         HT = case_when(HEIGHT >= 70 & HEIGHT < 77 ~ 1, 
                        TRUE ~ 0),
         LBW_NICU = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                NICU == 3 ~ 1, 
                              TRUE ~ 0),
         LBW_DAYS = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                BABYHOSP < 4 ~ 1, 
                              TRUE ~ 0),
         VAGASSIST= as.numeric(VAGASSIST),
         VAGASSIST_f = case_when(VAGASSIST %in% c(1,2) ~ 1, 
                                 TRUE ~ 0),
         GAFOOD = case_when(CURRENTFEEDC4 == 0 & BABY_SURVEYAGE_MONTHS >= 7  ~ 1, 
                            TRUE ~ 0),
         INC_MCAID = case_when(INCOME < 150000 & INCOME >= 100000 & 
                                 INSURC2 == 1 ~ 1, 
                               TRUE ~ 0),
         INC_PAN = case_when(INCOME >= 100000 & INCOME < 150000 & 
                               Source %in% c(4,6,8) ~ 1, 
                             TRUE ~ 0),
         NEED_F = case_when(SOCIALNEEDC1 == 1 ~ 1, 
                            SOCIALNEEDC2 == 1 ~ 1,
                            SOCIALNEEDC3 == 1 ~ 1,
                            SOCIALNEEDC4 == 1 ~ 1,
                            TRUE ~ 0),
         INC_NEEDS = case_when(INCOME>= 100000 & INCOME < 150000 & NEED_F == 1 ~ 1,
                               TRUE ~ 0),
         PRE_HYPER = case_when(PREPREG_PHYSCONDC1 == 1 ~ 1, 
                               TRUE ~ 0),
         PRE_DIABETES = case_when(PREPREG_PHYSCONDC2 == 1 ~ 1,
                                  TRUE ~ 0),
         LABCSEC = as.numeric(LABCSEC),
         CSECTIONTYPE = as.numeric(CSECTIONTYPE),
         CS_LABOR = case_when(LABCSEC == 1 & CSECTIONTYPE == 1 ~ 1, 
                              TRUE ~ 0),
         LABORINTC5 = as.numeric(LABORINTC5),
         VAGEXAM = case_when(LABORINTC5 == 0 ~ 1, 
                             TRUE ~ 0),
         TERM_NICU = case_when(GESTAGE_WEEKS >= 37 & NICU %in% c(1,2) ~ 1, 
                               TRUE ~ 0),
         PPTVISITS = case_when(PPVISIT == 99 ~ 1, 
                               TRUE ~ 0),
         MISSING_BW = case_when(is.na(BIRTHWEIGHT) ~ 1, 
                                TRUE ~ 0),
         MISSING_LABORHRS = case_when(is.na(as.numeric(LABORLENGTH)) ~ 1, 
                                      TRUE ~ 0),
         MISSING_MOMDAYS = case_when(is.na(DAYSHOSP)~ 1, 
                                     DAYSHOSP == 99 ~ 1,
                                     TRUE ~ 0),
         MISSING_BABYDAYS = case_when(is.na(BABYHOSP) ~ 1, 
                                      BABYHOSP == 99 ~ 1,
                                      TRUE ~ 0)) %>% 
  rowwise() %>%
  mutate(FLAG2 = sum(across(starts_with("F2_"))),
         FLAG3 = sum(across(starts_with("F3_"))), 
         SUM_FLAG_23 = sum(across(c("FLAG2", "FLAG3")))) %>% 
  ungroup()

# Overall ----
t_all <- LTM %>% 
  summarise(n = n(),
            MODE2023=round(sum(MODE2023 == 2)/n, 2),
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2),
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2),
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2),
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2),
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2),
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2),
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2),
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)) %>% 
  t() %>% 
  as.data.frame()
colnames(t_all) <- c("Full Population")

# Flag 1 (immediate removals) ----
t_1 <- LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  group_by(F1) %>% 
  summarise(n = n(), 
            MODE2023=round(sum(MODE2023 == 2)/n, 2),
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2),
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2),
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2),
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2),
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2),
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2),
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2),
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)) %>% 
  subset(F1 == 0) %>% 
  t() %>% as.data.frame()
t_1 <- t_1[-1,] %>% as.data.frame()
colnames(t_1) <- c("After Immediate Removals")
rownames(t_1) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                   "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                   "F3_PRE_DIABETES", "NO_PNC")

# Flag 2 (three strikes) ----
t_2 <- LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>% 
  mutate(F2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  group_by(F2) %>% 
  summarise(n = n(),
            MODE2023=round(sum(MODE2023 == 2)/n, 2),
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2),
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2),
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2),
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2),
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2),
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2),
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2),
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)) %>% 
  subset(F2 == 0) %>%
  t() %>% as.data.frame()
t_2 <- t_2[-1,] %>% as.data.frame()
rownames(t_2) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                   "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                   "F3_PRE_DIABETES", "NO_PNC")
colnames(t_2) <- c("Removed Flag 2s")

# Flag 3 ----
t_3 <- LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  subset(F1 == 0) %>% 
  mutate(F2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  rowwise() %>%
  mutate(all = sum(FLAG2, FLAG3)) %>%
  mutate(F3 = case_when(all < 6 ~ 0, TRUE ~ 1)) %>%
  subset(F2 == 0) %>%
  group_by(F3) %>% 
  summarise(n = n(),
            MODE2023=round(sum(MODE2023 == 2)/n, 2),
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2),
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2),
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2),
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2),
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2),
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2),
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2),
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)) %>% 
  subset(F3 == 0) %>%
  t() %>% as.data.frame()
t_3 <- t_3[-1,] %>% as.data.frame()
rownames(t_3) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                   "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                   "F3_PRE_DIABETES", "NO_PNC")
colnames(t_3) <- c("Flag 2&3 Removals")

# Merging together ----
all_results <- cbind(t_all, t_1, t_2, t_3)
all_results$NatDat <- c(NA, .32, .53, .40, .08, .03, .04, .03, .01, .02)
all_results$measure <- rownames(all_results)
all_results <- all_results %>% 
  subset(measure != "n") %>% 
  relocate(measure)

all_results %>% 
  gather(key = source, value = "prop",-measure) %>% 
  mutate(source = factor(source, 
                         levels = c("Full Population", "After Immediate Removals", 
                                    "Removed Flag 2s", "Flag 2&3 Removals", "NatDat")),
         measure = factor(measure, 
                          levels = c("MODE2023", "RELATIONSHIP","PARITY", 
                                     "F3_GAFOOD", "F3_VAGASSIST", "NO_PNC",
                                     "F3_EARLYPNC", "F3_PRE_HYPER", 
                                     "F3_PRE_DIABETES"))) %>% 
  ggplot(aes(x = measure, y = prop, group = source)) + 
  geom_col(aes(fill = source), position = "dodge") + 
  scale_x_discrete(labels = c("Cesarean", "Married", "1st Birth","Solid Food",
                              "Assisted Vaginal", "No Prenatal", 
                              "Early Prenatal", "Prepreg Hypertension", 
                              "Prepreg Diabetes")) + 
  scale_fill_manual(name = "Flag Stage",
                    values = sc_across(palette = "YGTVR", light = 3, sat = "muted")) + 
  theme_classic() + #coord_flip()
  theme(axis.text.x = element_text(angle = 35, vjust = .9, hjust = 1))


# Flag Check ----
F1_names <- LTM %>% 
  select(starts_with("F1")) %>% 
  colnames()

F2_names <- LTM %>% 
  select(starts_with("F2")) %>% 
  colnames()

F3_names <- LTM %>% 
  select(starts_with("F3")) %>% 
  colnames()

# Flag functions ----
check.flags <- function(flag, var){
  
  dat <- LTM %>% 
    group_by({{flag}}) %>% 
    summarise(n = sum({{var}} == 1))
  
  print(dat)
}

# Flag 1s -----
# "F1_NOTRIBALAFF", "F1_SPEED", "F1_USKG_ANY"  
LTM %>% 
  subset(F1_SPEED == 0) %>% 
  subset(SPEED == 1) %>% 
  # summarise(n = n())
  relocate(ConnectionDurationInMinutes, TotalDurationSec) %>%
  # View()
  nrow()

# select(c(UID2, ConnectionDurationInMinutes, FINAL_DETERMINATION)) %>% View()

check.flags(F1_NOTRIBALAFF, NOTRIBALAFF)

# Flags 2 and 3 ----
# "F2_ZIPCHECK","F2_GA_EXT",
# "F2_PNCB4LEARN","F2_HT_EXT", "F2_WT_EXT", 
# "F2_VLBNICU", "F2_VLBHOSP", "F2_DKREFCHECK"

# PROBLEMS: "F2_BW_EXT", "F2_MCAID_INC_EXT",
check.flags(F2_VLBHOSP, VLBHOSP)

# "F3_EARLYPNC","F3_NO_PNC","F3_PREGWT",
#  F3_HT","F3_VAGASSIST","F3_GAFOOD",
# "F3_INC_MCAID","F3_INC_PAN",
# "F3_PRE_HYPER","F3_PRE_DIABETES","F3_CS_LABOR",
# "F3_VAGEXAM","F3_PPTVISITS",
# "F3_MISSING_BW","F3_MISSING_LABORHRS","F3_MISSING_MOMDAYS",
# "F3_MISSING_BABYDAYS"

# PROBLEMS:# F3_WTDIFF, F3_LBW_NICU, F3_LBW_DAYS, F3_INC_NEEDS
# F3_TERM_NICU

check.flags(F3_MISSING_BABYDAYS, MISSING_BABYDAYS)

# Checking MDR flagging ----
options(scipen = 999)

# 37 F2_BW_EXT
F2_1 <- LTM %>% 
  subset(BW_EXT == 1 & F2_BW_EXT == 0) %>% 
  select(c(UID2, BIRTHWEIGHT, BW_EXT, F2_BW_EXT)) %>% 
  mutate(mismatch = "F2_BW_EXT")

# 35 F2_MCAID_INC_EXT -- 150000 borderline.
F2_2 <- LTM %>% 
  subset(F2_MCAID_INC_EXT == 0 & MCAID_INC_EXT == 1) %>% 
  select(c(UID2, INCOME, INSURC2, F2_MCAID_INC_EXT,
           MCAID_INC_EXT))%>% 
  mutate(mismatch = "F2_MCAID_INC_EXT")

# 34 F3_WTDIFF 
F3_1 <- LTM %>% 
  subset(F3_WTDIFF == 0 & WTDIFF == 1) %>% 
  select(c(UID2, PREG_WEIGHT, PREPREG_WEIGHT, 
           PREPREG_WEIGHT_A1, PREPREG_WEIGHT_B1, 
           PREGWEIGHT_A1, PREGWEIGHT_B1, F3_WTDIFF, WTDIFF))%>% 
  mutate(mismatch = "F3_WTDIFF")

# 51 F3_LBW_NICU
F3_2 <- LTM %>% 
  subset(F3_LBW_NICU == 0 & LBW_NICU == 1) %>% 
  select(c(UID2, BIRTHWEIGHT, BIRTHWEIGHT_LBS, BIRTHWEIGHT_LBSONLY, 
           BIRTHWEIGHT_OZ, BIRTHWEIGHT_G, NICU, 
           F3_LBW_NICU, LBW_NICU))%>% 
  mutate(mismatch = "F3_LBW_NICU")

# 68 F3_LBW_DAYS
F3_3 <- LTM %>% 
  subset(F3_LBW_DAYS == 0 & LBW_DAYS == 1) %>% 
  select(c(UID2, BABYHOSP, BIRTHWEIGHT, BIRTHWEIGHT_LBS, 
           BIRTHWEIGHT_OZ, BIRTHWEIGHT_G, BABYHOSP,
           F3_LBW_DAYS, LBW_DAYS))%>% 
  mutate(mismatch = "F3_LBW_DAYS")

# 53 F3_INC_NEEDS -- CHECK WITH NAN about income cut off 
F3_4 <- LTM %>% 
  subset(F3_INC_NEEDS == 0 & INC_NEEDS == 1) %>% 
  select(c(UID2, INCOME, SOCIALNEEDC1,SOCIALNEEDC2,
           SOCIALNEEDC3,SOCIALNEEDC4, 
           F3_INC_NEEDS, INC_NEEDS))%>% 
  mutate(mismatch = "F3_INC_NEEDS")

# 755 F3_TERM_NICU
F3_5 <- LTM %>% 
  subset(F3_TERM_NICU == 0 & TERM_NICU == 1) %>% 
  select(c(UID2, GESTAGE_WEEKS, NICU, F3_TERM_NICU, TERM_NICU))%>% 
  mutate(mismatch = "F3_TERM_NICU")

issues <- F2_1 %>% 
  full_join(F2_2) %>% 
  full_join(F3_1) %>% 
  full_join(F3_2) %>% 
  full_join(F3_3) %>% 
  full_join(F3_4) %>% 
  full_join(F3_5) %>% 
  relocate(UID2, mismatch) 

setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM")
write.csv(issues, "Algorithm_Issues.csv")

 