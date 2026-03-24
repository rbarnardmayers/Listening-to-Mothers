setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

# Planned vs. Unplanned not a repeat
r_svysummary(include = "CSECTIONTYPE", data = filter(LTM_dsn, xMODE2 == "Cesarean Primary"))

# VBAC rates by race and insurance 
r_svysummary(by = "INSURANCE", 
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

# Doula and Midwife by outcomes
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


