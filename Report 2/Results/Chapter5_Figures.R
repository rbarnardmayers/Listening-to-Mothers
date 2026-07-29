source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

#HCQUAL 
# RACE INSURANCE PARITY DISABILITY
# URBANICITY2 BIRTHATTEND2 MODE1INDEX
r_svysummary(by = "MODE1INDEX",
             include = "HCQUAL_R")

r_svysummary(by = "MODE1INDEX",
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

r_svysummary(include = "REPORT4", 
             data = filter(LTM_dsn, 
                           REPORT2 == "Yes, someone reported me to authorities" & 
                             REPORT3 == "Yes, there was an investigation"))


LTM_final %>% subset(REPORT3O != "") %>% 
  select(c(REPORT3O, REPORT3)) %>% View()

# TRAUMA
# MATTRAUMA
# TRAUMADETC7
# RACE INSURANCE MODE1INDEX  RESPONSIBTIER
r_svysummary(by = "RESPONSIBTIER", 
             include = "PROBABLEPTSD_R")


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

r_svysummary(include = c("xCARETIME_AVG", 
                         "xCAREDIST_AVG"))

# PPDISTANCE
r_svysummary(include = c("xPREDISTANCE_R", 
                         "xPOSTDISTANCE_R"))

# SUBGROUP DISTANCE 
# RACE INSURANCE URBANICITY2
r_svysummary(by = "RACE", 
             include = c("CAREDIST_AVG_10", 
                         "CAREDIST_AVG_20"))

# RACE INSURANCE URBANICITY2
r_svysummary(by = "RACE", 
             include = c("PREDISTANCE20", 
                         "POSTDISTANCE20"))
# RACE INSURANCE URBANICITY2
r_svysummary(by = "RACE", 
             include = c("PREDISTANCE10", 
                         "POSTDISTANCE10"))
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
r_svysummary(include = c('ERRORCONCERN_A1_R', 'ERRORCONCERN_A2_R',
                         'ERRORCONCERN_A3_R', 'ERRORCONCERN_A4_R',
                         'ERRORCONCERN_A5_R'))

r_svysummary(include = c('ERRORCONCERN_A3_R'),
             data = filter(LTM_dsn, NICU %in% c("Yes, for part of the time in the hospital", 
                                                "Yes, for the entire time in the hospital")))
# RACE RESPONSIBTIER
r_svysummary(by = "RESPONSIBTIER",
             include = c('ERRORCONCERN_A1_R', 'ERRORCONCERN_A2_R',
                         'ERRORCONCERN_A3_R', 'ERRORCONCERN_A4_R',
                         'ERRORCONCERN_A5_R'))


# CARECONC
r_svysummary(by = "RACE", 
             include = c("CARECONC_R", 'CARECONC1_R'))

# RACE PROVIDER2_R MARRIED 
r_svysummary(#by = "MARRIED", 
             include = c("CARECONC4_R", 'CARECONC5_R'))


# ResLanguage LANGSIMP ENGPROFSIMP
r_svysummary(by = "ENGPROFSIMP", 
             include = c("CARECONC2_R", "CARECONC3_R"))


# INTERPRET_R INTERPRET1_R
# ENGPROFSIMP LANGSIMP
r_svysummary(by = "LANGSIMP", 
             include = c("INTERPRET_R", "INTERPRET1_R"))


# FUTURE BIRTH
# RACE PARITY CURRREL AGE4 
r_svysummary(by = "RACE",
             include = "xFUTUREBIRTH", 
             data = filter(LTM_dsn, xFUTUREBIRTH != "I'm not sure"))

# INSURCURR
#Prenatal
r_svysummary(include = c('INSURQ2C1', 'INSURQ2C2', 'INSURQ2C3', 'INSURQ2C4', 
                         'INSURQ2C5', 'INSURQ2C6'))
# Childbirth
r_svysummary(include = c('INSURC1', 'INSURC2', 'INSURC3', 'INSURC4', 'INSURC5', 
                         'INSURC6'))
# Newborn
r_svysummary(include = c('INSURBABYC1', 'INSURBABYC2', 'INSURBABYC3', 
                         'INSURBABYC4', 'INSURBABYC5', 'INSURBABYC6'))
# Current baby
r_svysummary(include = c('INSURCURRBABYC1', 'INSURCURRBABYC2', 
                         'INSURCURRBABYC3', 'INSURCURRBABYC4', 'INSURCURRBABYC5',
                         "INSURCURRBABYC6"))
# Current mom
r_svysummary(include = c('INSURCURRC1', 'INSURCURRC2', 'INSURCURRC3', 
                         'INSURCURRC4', 'INSURCURRC5', 'INSURCURRC6'))

# FUTURE BABY BIRTH
r_svysummary(include = c('FUTUREBABYDIFF1_A1R', 'FUTUREBABYDIFF1_A2R', 
                         'FUTUREBABYDIFF1_A3R', 'FUTUREBABYDIFF1_A4R'), 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A1R - MIDWIFE
# RACE, INSURANCE, PROVIDER2_R, BIRTHATTEND_R
r_svysummary(by = "PROVIDER2_R", 
             include = "FUTUREBABYDIFF1_A1R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# MIDWIFE CONCERN
r_svysummary(include = c("MIDWIFECONCERNC1", 'MIDWIFECONCERNC2', 
                          'MIDWIFECONCERNC3', 'MIDWIFECONCERNC4', 
                          'MIDWIFECONCERNC5', 'MIDWIFECONCERNC6', 
                          'MIDWIFECONCERNC7'))

# FUTUREBABYDIFF1_A4R - DOULA
# RACE, INSURANCE, RESPONSIBTIER, DOULAANY
r_svysummary(by = "DOULAANY", 
             include = "FUTUREBABYDIFF1_A4R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A2R - Birth center
# RACE, INSURANCE, BIRTHCOUNTRYUS
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A2R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A3R - Home birth
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A3R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# WICBEN 
# CURRREL_R
r_svysummary(by = "DISABILITY", 
             include = "WICBEN")

# WICPP
# RACE INSURANCE PARITY CURRREL_R DISABILITY
r_svysummary(by = "RACE",
             include = "WICPP_ANY")

r_svysummary(by = "PARITY",
             include = c("WICPPC1", "WICPPC2",
                         "WICPP_ANY", 'WICPP_ONLY'))

# WICOFFER 
r_svysummary(include = c('WICOFFER_R', 'WICFEEDING_A1', 'WICFEEDING_A2', 
                         'WICFEEDING_A3', 'WICFEEDING_A4'), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding") & 
                             WICANY == 1))

r_svysummary(by = "MODE1INDEX", 
             include = c('WICOFFER_R'), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding") & 
                             WICANY == 1))

# RESOURCEVALUE
LTM_final %>% 
  filter(RESOURCEVALUE_A12 %in% c("Not valuable", 
                                  "Somewhat valuable", 
                                  "Very valuable")) %>% 
  nrow()

r_svysummary(include = c(paste0("RESOURCEVALUE_A", 1:12)))
r_svysummary(include = "RESOURCEVALUE_A12", 
             data = filter(LTM_dsn, 
                           RESOURCEVALUE_A12 %in% c("Not valuable", 
                                                   "Somewhat valuable", 
                                                   "Very valuable")))

# RESOURCEVALUE
LTM_final %>% 
  filter(RESOURCEVALUE2_A10 %in% c("Not valuable", 
                                  "Somewhat valuable", 
                                  "Very valuable")) %>% 
  nrow()

r_svysummary(include = c(paste0("RESOURCEVALUE2_A", 1:10)))

r_svysummary(include = "RESOURCEVALUE2_A10", 
             data = filter(LTM_dsn, 
                           RESOURCEVALUE2_A10 %in% c("Not valuable", 
                                                    "Somewhat valuable", 
                                                    "Very valuable")))

# 5.RESOURCETRUST
r_svysummary(include = paste0("RESOURCETRUST_A", 1:21, "_N"),
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding"))

# COVID
# RACE, INSURANCE, URBANICITY2 DISABILITY
r_svysummary(by = "RACE", 
             include = "ANYCOVID")

# COVIDVAC 
# RACE, INSURANCE, DISABILITY, POLIT2
r_svysummary(by = "POLIT2", 
             include = "COVIDVACC_ANY")

# RACE, INSURANCE, DISABILITY, POLIT2
r_svysummary(#by = "RACE", 
             include = "COVIDVACC2")
# BABYVAC
# CURRREL_R POLIT2
r_svysummary(by = "POLIT2",
             include = "BABYVAC")

# CLIMATE
# CENSUS_REG
r_svysummary(#by = "CENSUS_REG", 
             include = paste0("CLIMATEC", 1:9))

