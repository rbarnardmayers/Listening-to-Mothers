setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

# Birth attendant
r_svysummary(include = "BIRTHATTEND")
r_svysummary(by = "xMODE1",
             include = "BIRTHATTEND")

# Table 4.2
r_svysummary(by = "DOULAC2", 
             include = c("xMODE1", "xMODE2"))

# Planned vs. Unplanned not a repeat
r_svysummary(include = "CSECTIONTYPE", data = filter(LTM_dsn, xMODE2 == "Cesarean Primary"))
r_svysummary(include = "CSECTIONTYPE", data = filter(LTM_dsn, xMODE2 == "Cesarean Repeat"))

# VBAC rates by race and insurance 
r_svysummary(by = "BIRTHATTEND2", 
             include = "xMODE2", 
             data = filter(LTM_dsn, xMODE2 %in% c("Cesarean Repeat", "VBAC")))

# Pie chart VBAC interest 
r_svysummary(include = "VBACINTEREST")
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

# Unplanned Reason by induction
r_svysummary(by = "MEDINDUCE", 
             include = "UNPLANNEDREASON", 
             data = filter(LTM_dsn, CSECTIONTYPE == "Unplanned"))

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


# Big baby 
r_svysummary(by = "BIGBABY1",
             include = "MODE2023", 
             data = filter(LTM_dsn, PARITY == "Nulliparous"))

r_svysummary(by = "MODE2023",
             include = "MACROSOMIC", 
             data = filter(LTM_dsn, BIGBABY2 == "Yes, a C-section"))

# TABLES ----
# practices during labor
r_svysummary(by = "CSECTIONTYPE_R", 
             include = c('BIRTHATTEND2', 'DOULAC2', 'VAGEXAM_5', 'FETALMONC1', 
                         'MEDINDUCE', 'LABORWALK'), 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))

# BIRTHATTEND2, DOULAC2, VAGEXAM_5, FETALMONC1, MEDINDUCE, LABORWALK

# Table 4.4 
r_svysummary(include = c("MODE2023", "xMODE2"))
r_svysummary(by = "PRIOR_C", 
             include = "MODE2023")

