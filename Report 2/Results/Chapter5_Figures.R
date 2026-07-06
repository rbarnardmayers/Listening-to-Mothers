source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

#HCQUAL 
# RACE INSURANCE PARITY DISABILITY
# URBANICITY2 BIRTHATTEND2 MODE1INDEX
r_svysummary(by = "MODE1INDEX",
             include = "HCQUAL_R")

r_svysummary(by = "DISABILITY",
             include = "QUALCARE_R")


# REPORT, REPORT2, REPORT3
r_svysummary(include = "REPORT", 
             data = filter(LTM_dsn, 
                           REPORT != "I’d prefer not to answer"))

r_svysummary(by = "REPORT", 
             include = "REPORT2", 
             data = filter(LTM_dsn, 
                           REPORT2 != "I’d prefer not to answer"))

r_svysummary(include = "REPORT3", 
             data = filter(LTM_dsn, 
                           REPORT2 == "Yes, someone reported me to authorities" & 
                             REPORT3 != "I’d prefer not to answer"))

LTM_final %>% subset(REPORT3O != "") %>% 
  select(c(REPORT3O, REPORT3)) %>% View()

# TRAUMA
# MATTRAUMA
# TRAUMADETC7

# RACE INSURANCE MODE1INDEX 
r_svysummary(by = "RACE",
             include = "MATTRAUMA")

r_svysummary(include = c('TRAUMADETC1', 'TRAUMADETC2', 'TRAUMADETC3', 
                         'TRAUMADETC4', 'TRAUMADETC5'), 
             data = filter(LTM_dsn, MATTRAUMA == "Yes"))

# HOSPDISTANCE
max(LTM_final$HOSPDISTANCE, na.rm = T)

# PPTIME
r_svysummary(include = c("xPPCTIME", 
                         "xPNCTIME"))

# PPDISTANCE
r_svysummary(include = c("xPREDISTANCE_R", 
                         "xPOSTDISTANCE_R"))

# SUBGROUP DISTANCE 
# RACE INSURANCE URBANICITY2
r_svysummary(by = "RACE", 
             include = c("PREDISTANCE20", 
                         "POSTDISTANCE20"))

# INTENDLOCALE_R
# RACE, INSURANCE, PARITY, DISABILITY2, URBANICITY2
r_svysummary(by = "URBANICITY2", 
             include = "INTENDLOCALE_R")
# FUTUREBIRTH
# RACE, PARITY, CURRREL, AGE4
r_svysummary(by = "AGE4",
             include = "ANYKIDS")

# COSTMOM
r_svysummary(by = "xCOSTMOM", 
             include = "xCOSTMOM1")

# COSTBABY
r_svysummary(by = "xCOSTBABY", 
             include = "xCOSTBABY1")

# ERRORCONCERN
# RACE RESPONSIBTIER
r_svysummary(by = "RESPONSIBTIER",
             include = c('ERRORCONCERN_A1_R', 'ERRORCONCERN_A2_R',
                         'ERRORCONCERN_A3_R', 'ERRORCONCERN_A4_R',
                         'ERRORCONCERN_A5_R'))


# CARECONC
# ResLanguage LANGSIMP ENGPROFSIMP
r_svysummary(by = "ResLanguage", 
             include = "CARECONC2")

r_svysummary(by = "ResLanguage", 
             include = "CARECONC3")


# FUTURE BIRTH
# RACE PARITY CURRREL AGE4 
r_svysummary(by = "RACE",
             include = "xFUTUREBIRTH", 
             data = filter(LTM_dsn, xFUTUREBIRTH != "I'm not sure"))

