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
         NO_PNC = case_when(LEARNED2 == 1 ~ 1, 
                               TRUE ~ 0),
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
         WEIGHT_f1 = case_when(as.numeric(PREPREG_WEIGHT)*12 + as.numeric(PREPREG_WEIGHT) < 94 ~ 1, 
                               as.numeric(PREPREG_WEIGHT)*12 + as.numeric(PREPREG_WEIGHT) > 317 ~ 1, 
                               TRUE ~ 0),
         BIRTHWEIGHT_LBS = as.numeric(BIRTHWEIGHT_LBS),
         BIRTHWEIGHT_G = as.numeric(BIRTHWEIGHT_G),
         BIRTHWEIGHT_OZ = as.numeric(BIRTHWEIGHT_OZ),
         BIRTHWEIGHT_OZ = case_when(is.na(BIRTHWEIGHT_OZ) ~ 0, 
                                    TRUE ~ BIRTHWEIGHT_OZ),
         BIRTHWEIGHT = case_when(!is.na(BIRTHWEIGHT_LBS) ~ BIRTHWEIGHT_LBS*453.592 + BIRTHWEIGHT_OZ*28.3495,
                                 !is.na(BIRTHWEIGHT_G) ~ BIRTHWEIGHT_G), 
         BIRTHWEIGHT = as.numeric(BIRTHWEIGHT),
         GA_EXT = case_when(as.numeric(GESTAGE_WEEKS <= 27 ~ 1, 
                                       as.mumeric(GESTAGE_WEEKS >= 42 ~ 1, 
                                                  TRUE ~ 0))),
         HT_EXT = case_when(as.numeric(HEIGHT_FEET)*12 + as.numeric(HEIGHT_INCHES) < 57 ~ 1, 
                          as.numeric(HEIGHT_FEET)*12 + as.numeric(HEIGHT_INCHES) > 77 ~ 1, 
                          TRUE ~ 0),
         WT_EXT = case_when(as.numeric(PREG_WEIGHT) < 115 ~ 1, 
                            as.numeric(PREG_WEIGHT) > 340 ~ 1, 
                            TRUE ~ 0),
         BW_EXT = case_when(BIRTHWEIGHT < 970 ~ 1, 
                            BIRTHWEIGHT > 4620 & PREPREG_PHYSCONDC2 == 1 ~ 1,
                            BIRTHWEIGHT > 4620 & PREGCONDITIONC1 == 1 ~ 1,
                            BIRTHWEIGHT > 4800 & PREPREG_PHYSCONDC2 == 0 ~ 1,
                            BIRTHWEIGHT > 4800 & PREGCONDITIONC1 == 0 ~ 1,
                            TRUE ~ 0),
         MCAID_INC_EXT = case_when(INCOME > 150000 & INSURC2 == 1 ~ 1, 
                                   TRUE ~ 0), 
         VLBNICU = case_when(BIRTHWEIGHT < 1500 & NICU == 3 ~ 1, 
                             TRUE ~ 0), 
         VLBHOSP = case_when(BIRTHWEIGHT < 1500 & BABYHOSP< 4 ~ 1, 
                             TRUE ~ 0), 
         CURRWEIGHT_KG = as.numeric(CURRWEIGHT_KG),
         PREGWEIGHT_B1 = as.numeric(PREGWEIGHT_B1),
         PREPREG_WEIGHT_B1 = as.numeric(PREPREG_WEIGHT_B1),
         KG_any = case_when(!is.na(CURRWEIGHT_KG)  &  CURRWEIGHT_KG != 0~ 1, 
                            !is.na(PREGWEIGHT_B1) &  PREGWEIGHT_B1 != 0 ~ 1, 
                            !is.na(PREPREG_WEIGHT_B1) & 
                              PREPREG_WEIGHT_B1 != 0 ~ 1, 
                            TRUE ~ 0), 
         USKGANY = case_when(BIRTHCOUNTRY == 1 & KG_any == 1 ~ 1, 
                             TRUE ~ 0),
         # DKREFCHECK = case_when(), 
         LEARNED1 = case_when(LEARNED1 == 99 ~ NA, TRUE ~ LEARNED1), 
         PNCB4LEARN = case_when(as.numeric(LEARNED2) < as.numeric(LEARNED1) ~ 1, 
                                TRUE ~ 0),
         EARLYPNC = case_when(LEARNED2 <= 4 ~ 1, 
                              TRUE ~ 0),
         PREGWT = case_when(PREG_WEIGHT < 129 & WEIGHT_f == 0 ~ 1, 
                            TRUE ~ 0),
         WTDIFF = case_when(PREG_WEIGHT < PREPREG_WEIGHT ~ 1, 
                            TRUE ~ 0 ),
         HEIGHT = (HEIGHT_FEET*12) + HEIGHT_INCHES,
         HEIGHT_2 = case_when(HEIGHT > 70 & HEIGHT <= 77 ~ 1, 
                              TRUE ~ 0),
         LBW_NICU = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                NICU == 3 ~ 1, 
                              TRUE ~ 0),
         LBW_DAYS = case_when(BIRTHWEIGHT >= 1500 & BIRTHWEIGHT < 2500 & 
                                BABYHOSP < 4 ~ 1, 
                              TRUE ~ 0),
         VAGASSIST_f = case_when(VAGASSIST %in% c(1,2) ~ 1, 
                                 TRUE ~ 0),
         GAFOOD = case_when(CURRENTFEEDC4 == 1 & BABY_SURVEYAGE_MONTHS >= 7  ~ 1, 
                            TRUE ~ 0),
         INC_MCAID = case_when(INCOME <= 150000 & INCOME > 100000 & 
                                 INSURC2 == 1 ~ 1),
         INC_PAN = case_when(INCOME > 100000 & Source %in% c(4,6) ~ 1, 
                             TRUE ~ 0),
         NEED_F = case_when(SOCIALNEEDC1 == 1 ~ 1, 
                            SOCIALNEEDC2 == 1 ~ 1,
                            SOCIALNEEDC3 == 1 ~ 1,
                            SOCIALNEEDC4 == 1 ~ 1,
                            SOCIALNEEDC5 == 1 ~ 1,
                            SOCIALNEEDC6 == 1 ~ 1,
                            TRUE ~ 0),
         INC_NEEDS = case_when(INCOME> 100000 & NEED_F == 1 ~ 1,
                               TRUE ~ 0),
         PRE_HYPER = case_when(PREPREG_PHYSCONDC1 == 1 ~ 1, 
                               TRUE ~ 0),
         PRE_DIABETES = case_when(PREPREG_PHYSCONDC2 == 1 ~ 1,
                                  TRUE ~ 0),
         CS_LABOR = case_when(as.numeric(LABCSEC) == 1 ~ 1, 
                              TRUE ~ 0),
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
                                     TRUE ~ 0),
         MISSING_BABYDAYS = case_when(is.na(BABYHOSP) ~ 1, 
                                      TRUE ~ 0)) %>% 
  rowwise() %>%
  mutate(FLAG3 = sum(across(starts_with("F3_"))), 
         SUM_FLAG_23 = sum(FLAG2, FLAG3)) %>% 
  ungroup()

# Overall ----
t_all <- LTM %>% 
  summarise(n = n(),
            MODE2023=round(sum(MODE2023 == 2)/n, 2)*100,
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2)*100,
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2)*100,
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2)*100,
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2)*100,
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2)*100,
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2)*100,
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2)*100,
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)*100) %>% 
  t() %>% 
  as.data.frame()
colnames(t_all) <- c("Overall")

# Flag 1 (immediate removals) ----
t_1 <- LTM %>% 
  mutate(F1 = case_when(FLAG1 == 0 ~ 0, TRUE ~ 1)) %>%
  group_by(F1) %>% 
  summarise(n = n(), 
            MODE2023=round(sum(MODE2023 == 2)/n, 2)*100,
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2)*100,
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2)*100,
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2)*100,
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2)*100,
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2)*100,
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2)*100,
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2)*100,
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)*100) %>% 
  subset(F1 == 0) %>% 
  t() %>% as.data.frame()
t_1 <- t_1[-1,] %>% as.data.frame()
colnames(t_1) <- c("Removals")
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
            MODE2023=round(sum(MODE2023 == 2)/n, 2)*100,
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2)*100,
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2)*100,
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2)*100,
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2)*100,
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2)*100,
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2)*100,
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2)*100,
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)*100) %>% 
  subset(F2 == 0) %>%
  t() %>% as.data.frame()
t_2 <- t_2[-1,] %>% as.data.frame()
rownames(t_2) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                   "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                   "F3_PRE_DIABETES", "NO_PNC")
colnames(t_2) <- c("Inconsistent")

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
            MODE2023=round(sum(MODE2023 == 2)/n, 2)*100,
            RELATIONSHIP=round(sum(RELATIONSHIP == 1)/n, 2)*100,
            PARITY=round(sum(PARITY == "Nulliparous")/n, 2)*100,
            F3_GAFOOD=round(sum(F3_GAFOOD == 1)/n, 2)*100,
            F3_VAGASSIST=round(sum(F3_VAGASSIST == 1)/n, 2)*100,
            F3_EARLYPNC=round(sum(F3_EARLYPNC == 1)/n, 2)*100,
            F3_PRE_HYPER=round(sum(F3_PRE_HYPER == 1)/n, 2)*100,
            F3_PRE_DIABETES=round(sum(F3_PRE_DIABETES == 1)/n, 2)*100,
            NO_PNC=round(sum(LEARNED2 == 1)/n, 2)*100) %>% 
  subset(F3 == 0) %>%
  t() %>% as.data.frame()
t_3 <- t_3[-1,] %>% as.data.frame()
rownames(t_3) <- c("n","MODE2023","RELATIONSHIP","PARITY","F3_GAFOOD" ,
                   "F3_VAGASSIST","F3_EARLYPNC","F3_PRE_HYPER",
                   "F3_PRE_DIABETES", "NO_PNC")
colnames(t_3) <- c("Unlikely")

# Merging together ----
all_results <- cbind(t_all, t_1, t_2, t_3)
all_results$NatDat <- c(NA, 32, 53, 40, 8, 3, 4, 3, 1, 2)
all_results$measure <- rownames(all_results)
all_results <- all_results %>% 
  subset(measure != "n") 

all_results %>% 
  gather(key = source, value = "prop",-measure) %>% 
  mutate(source = factor(source, 
                         levels = c("Overall", "Removals", 
                                    "Inconsistent", "Unlikely", "NatDat")),
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
  group_by(F1_NOTRIBALAFF) %>% 
  summarise(n = n(), 
            no_tribe = sum(AIAN == 99), 
            tribe = sum(AIAN != 99))

LTM %>% 
  group_by(F1_SPEED) %>% 
  summarise(mean = mean(ConnectionDurationInMinutes, na.rm = T), 
            min = min(ConnectionDurationInMinutes, na.rm =T), 
            max = max(ConnectionDurationInMinutes, na.rm =T))

LTM %>% 
  subset(F1_SPEED == 0) %>% 
  mutate(TIME = case_when(ConnectionDurationInMinutes < 19 ~ 1, 
                          ConnectionDurationInMinutes >= 19 ~ 0)) %>% 
  subset(TIME == 1) %>% summarise(n = n())
# select(c(UID2, ConnectionDurationInMinutes, FINAL_DETERMINATION)) %>% View()

LTM %>% 
  group_by(F1_USKG_ANY) %>% 
  summarise(n = sum(USKGANY == 1, na.rm = T)) 

# Flags 2 and 3 ----
# "F2_ZIPCHECK","F2_GA_EXT",
# "F2_PNCB4LEARN","F2_HT_EXT",
# "F2_WT_EXT", 
#  "F2_BW_EXT", "F2_MCAID_INC_EXT","F2_VLBNICU",
# "F2_VLBHOSP", "F2_DKREFCHECK",
check.flags(F2_VLBHOSP, VLBHOSP)

# TO CHECK ----
# BW_EXT

# "F3_EARLYPNC","F3_NO_PNC","F3_PREGWT",
# "F3_WTDIFF","F3_HT","F3_LBW_NICU",
# "F3_LBW_DAYS","F3_VAGASSIST","F3_GAFOOD",
# "F3_INC_MCAID","F3_INC_PAN","F3_INC_NEEDS",
# "F3_PRE_HYPER","F3_PRE_DIABETES","F3_CS_LABOR",
# "F3_VAGEXAM","F3_TERM_NICU","F3_PPTVISITS",
# "F3_MISSING_BW","F3_MISSING_LABORHRS","F3_MISSING_MOMDAYS",
# "F3_MISSING_BABYDAYS"

check.flags(F3_MISSING_BABYDAYS, MISSING_BABYDAYS)

# 154 with no prenatal care
# 34 with wt diff 
# F3_HT 12 not matched
# 51 with LBW NICU
# 68 with few hosp days
# 67 F3_INC_PAN
# 104 F3_INC_NEEDS
# 413 F3_CS_LABOR
# 749 F3_TERM_NICU

# TO CHECK  ----
# VAGASSIST
# GAFOOD
# INC_MCAID
# VAGEXAM
# MISSING_MOMDAYS
# MISSING_BABYDAYS
