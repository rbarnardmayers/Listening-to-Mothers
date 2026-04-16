setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

# For carol 
r_svysummary(include = "NUM_CS",
            data = filter(LTM_dsn, PRIOR_C == 1))

# Birth attendant
r_svysummary(include = "BIRTHATTEND")
r_svysummary(by = "xMODE1",
             include = "BIRTHATTEND")

# Table 4.1
count_svysummary(by = "xMODE2", 
                 include = "CSECTIONTYPE")

r_svysummary(include = "CSECTIONTYPE_R2")
r_svysummary(by = "PROVIDER2",
                 include = "CSECTIONTYPE_R2")


# Planned vs. Unplanned not a repeat
count_svysummary(include = "CSECTIONTYPE", 
             data = filter(LTM_dsn,
                          xMODE2 == "Cesarean Primary"))
# planned: 158061.7800/3396402
# unplanned: 452065.5700/3396402

count_svysummary(include = "CSECTIONTYPE", 
             data = filter(LTM_dsn, 
                           xMODE2 == "Cesarean Repeat"))
# planned: 331157.8600/3396402
# unplanned: 112156.7700/3396402
r_svysummary(by = "xMODE2", 
             include = "CSECTIONTYPE")

# VBAC rates by race and insurance 
r_svysummary(by = "PROVIDER2", 
             include = "xMODE2", 
             data = filter(LTM_dsn, xMODE2 %in% c("Cesarean Repeat", "VBAC")))

r_svysummary(include = "CSECTIONTYPE")
# Planned cesarean subgroups and any significance
# RE
# Prenatal provider
# (no data but a Q about birth attendant: pre-recode, how is this 11% mw?)
# Parity
# 
# Unplanned cesarean subgroups and any significance
# RE
# Prenatal provider
# Birth attendant
# Prenatal doula
# Birth doula
# Parity
# Relationship
r_svysummary(by = "RACE", 
             include = "CSECTIONTYPE")

fig_compile(maincol = "CSECTIONTYPE", 
            others = c("RACE", "INSURANCE", "PROVIDER2",
                       "BIRTHATTEND2", "DOULAC1",
                       "PARITY", "MARRIED")) %>% 
  View()

# Pie chart VBAC interest 
r_svysummary(include = "VBACINTEREST", 
             data = filter(LTM_dsn, 
                           xMODE2 == "Cesarean Repeat"))
r_svysummary(by = "VBACINTEREST", include = "VBACCHOICE")
r_svysummary(include = c("VBACACCESSC1", "VBACACCESSC2","VBACACCESSC3"), 
             data = filter(LTM_dsn, VBACINTEREST == "Yes" & VBACCHOICE == "No"))

# Planned Repeat cesarean by race and insurance
r_svysummary(by = "INSURANCE", 
             include = "xMODE2", 
             data = filter(LTM_dsn, CSECTIONTYPE == "Planned ahead of time and scheduled before you went into labor"))

# unplanned cesareans by race, insurance, doula, midwife
r_svysummary(by = "BIRTHATTEND", 
             include = "CSECTIONTYPE")

r_svysummary(by = "PROVIDER2",
             include = "MODE2023", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))

# Unplanned Reason by induction
r_svysummary(#by = "MEDINDUCE", 
             include = "UNPLANNEDREASON", 
             data = filter(LTM_dsn, CSECTIONTYPE == "Unplanned"))

# Planned reason
r_svysummary(include = "PLANNEDC", 
             data = filter(LTM_dsn, CSECTIONTYPE == "Planned ahead of time and scheduled before you went into labor"))

# VBAC Effort by success
r_svysummary(by = "VBACCHOICE",
             include = "VBACEFFORT")

r_svysummary(by = "VBACEFFORT2", 
             include = "ANYLABOR")

# Doula and Midwife by outcomes
LTM_dsn %>% 
  filter(MODE2023 == "Vaginal birth" | LABCSEC == "Yes") %>%
  tbl_svysummary(by = "DOULAC2", 
                 include = c("MODE2023", "CSECTIONTYPE"))

r_svysummary(include = c("CSECTIONTYPE", "CUM_ASS"))
r_svysummary( include = c("xMODE2"), data = filter(LTM_dsn, xMODE2 %in% c("VBAC", "Cesarean Repeat")))

r_svysummary(by = "TRI_DOULA", include = c("CSECTIONTYPE", "CUM_ASS"))
r_svysummary(by = "TRI_DOULA", include = c("xMODE2"), data = filter(LTM_dsn, xMODE2 %in% c("VBAC", "Cesarean Repeat")))

r_svysummary(by = "DOULAC1", include = c("CSECTIONTYPE", "CUM_ASS"))
r_svysummary(by = "DOULAC1", include = c("xMODE2"), data = filter(LTM_dsn, xMODE2 %in% c("VBAC", "Cesarean Repeat")))

r_svysummary(by = "DOULAC2", include = c("CSECTIONTYPE", "CUM_ASS"))
r_svysummary(by = "DOULAC2", include = c("xMODE2"), data = filter(LTM_dsn, xMODE2 %in% c("VBAC", "Cesarean Repeat")))

r_svysummary(by = "PROVIDER2", include = c("CSECTIONTYPE", "CUM_ASS"))
r_svysummary(by = "PROVIDER2", include = c("xMODE2"), data = filter(LTM_dsn, xMODE2 %in% c("VBAC", "Cesarean Repeat")))

# Assisted Vaginal
fig.47 <- fig_compile("ASSISTED")
fig.472 <- fig_compile("ASSISTED", 
                       # others = c("PARITY"))
                       # others = c("PROVIDER2"))
                       others = c("BIRTHATTEND2"))

r_svysummary(by = "RACE", 
             include = "PAINMEDSC7", 
             data = filter(LTM_dsn, xMODE1 == "Vaginal all"))
# Big baby 
r_svysummary(by = "BIGBABY1", 
             include = "xMODE1")

r_svysummary(by = "BIGBABY1",
             include = "MODE2023", 
             data = filter(LTM_dsn, PARITY == "Nulliparous"))

r_svysummary(by = "BIGBABY2", 
             include = "MODE2023")

r_svysummary(include = "CSECTIONTYPE", 
             data = filter(LTM_dsn, BIGBABY2 == "Yes, a C-section"))

r_svysummary(by = "MODE2023",
             include = "MACROSOMIC", 
             data = filter(LTM_dsn, BIGBABY2 == "Yes, a C-section"))

# TABLES ----
# Table 4.1 
count_svysummary(by = "xMODE2", 
                 include = "CSECTIONTYPE")
count_svysummary(by = "xMODE2", 
                 include = "VAGASSIST")

# Table 4.2
r_svysummary(by = "SUM_LABORSTUFF", 
             include = "CSECTIONTYPE_R")

LTM_final %>% 
  subset(SUM_LABORSTUFF %in% c("5", "6", "7")) %>% 
   select(c(DOULAC1, DOULAC2, FETALMONC2_R, VAGEXAM_5, 
            LABORWALK_R, LABORPERMIT_A1_R, LABORPERMIT_A2_R,
            PROVIDER_R, SUM_LABORSTUFF,UNPLANNEDREASON)) %>%
  View()

# BIRTHATTEND2, DOULAC2, VAGEXAM_5, FETALMONC1, MEDINDUCE, LABORWALK

# Table 4.4 
r_svysummary(include = c("MODE2023", "xMODE2"))
r_svysummary(by = "PRIOR_C", 
             include = "MODE2023")

