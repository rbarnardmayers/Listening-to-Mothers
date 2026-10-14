source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

#HCQUAL 
# RACE INSURANCE PARITY DISABILITY
# URBANICITY2 BIRTHATTEND2 MODE1INDEX
r_svysummary(by = "RACE",
             include = "HCQUAL_R")

r_svysummary(by = "MODE1INDEX",
             include = "QUALCARE_R")

r_svysummary(include = c("HCQUAL", "QUALCARE"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

count_svysummary(#by = "HCQUAL",
  include = "QUALCARE",
  data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

# REPORT, REPORT2, REPORT3

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
# RACEC1 RACE_NOTWHITE RACE MULTI_NOWHITE_R
r_svysummary(by = "RACEC1",
             include = "REPORT")

r_svysummary(by = "RACEC1",
             include = "REPORT2")

r_svysummary(by = "RACEC1",
             include = "REPORT3", 
             data = filter(LTM_dsn,
                           REPORT2 == "Yes, someone reported me to authorities"))

r_svysummary(by = "RACEC1",
             include = "REPORT4", 
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
summary(LTM_final$HOSPDISTANCE, na.rm = T)

r_svysummary(by = "URBANICITY2",
             include = "HOSPDISTANCE")

summary(LTM_final$HOSPTIME_R)

r_svysummary(by = "URBANICITY2",
             include = "HOSPTIME_R")

# PPTIME
r_svysummary(include = c("xPPCTIME", 
                         "xPNCTIME"))

r_svysummary(by = "URBANICITY2", 
             include = c("CARETIME_AVG", 
                         "CAREDIST_AVG"))

# PPDISTANCE
r_svysummary(include = c("xPREDISTANCE_R", 
                         "xPOSTDISTANCE_R"))

# SUBGROUP DISTANCE 
# RACE INSURANCE URBANICITY2
r_svysummary(by = "RACE", 
             include = c("CAREDIST_AVG_10", 
                         "CAREDIST_AVG_20"))

# TRANSPORT
r_svysummary(include = c("PRETRANSPORT_R", "POSTTRANSPORT_R"))

r_svysummary(by = "INSURANCE",
             include = "TRANSPORT_CAR",
             data = filter(LTM_dsn, Q2FIELD == "Original Fielding"))


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

r_svysummary(include = c('ERRORCONCERN_A4_R'))

r_svysummary(include = c('ERRORCONCERN_A3_R'),
             data = filter(LTM_dsn, NICU %in% c("Yes, for part of the time in the hospital", 
                                                "Yes, for the entire time in the hospital")))
# RACE RESPONSIBTIER
r_svysummary(by = "RESPONSIBTIER",
             include = c('ERRORCONCERN_A1_R', 'ERRORCONCERN_A2_R',
                         'ERRORCONCERN_A3_R', 'ERRORCONCERN_A4_R',
                         'ERRORCONCERN_A5_R'))


# CARECONC
r_svysummary(#by = "RACE", 
  include = c("CARECONC_R"))

r_svysummary(by = "RACE",
             include = "CARECONC1_R", 
             data = filter(LTM_dsn, CARECONC_R == "Important"))

# RACE PROVIDER2_R  
r_svysummary(#by = "MARRIED", 
  include = c("CARECONC4_R"))

r_svysummary(by = "PROVIDER2_R",
             include = "CARECONC5_R", 
             data = filter(LTM_dsn, CARECONC4_R == "Important"))

# xResLanguage ENGPROFSIMP
r_svysummary(by = "ENGPROFSIMP", 
             include = c("CARECONC2_R"), 
             data = filter(LTM_dsn, LANGHOMEC1 == "Not selected"))

r_svysummary(by = "ENGPROFSIMP",
             include = "CARECONC3_R", 
             data = filter(LTM_dsn, LANGHOMEC1 == "Not selected" & 
                             CARECONC2_R == "Important"))

# INTERPRET_R INTERPRET1_R
# ENGPROFSIMP LANGSIMP
r_svysummary(by = "ENGPROFSIMP", 
             include = c("INTERPRET_R"), 
             data = filter(LTM_dsn, LANGHOMEC1 == "Not selected"))

r_svysummary(by = "ENGPROFSIMP", 
             include = c("INTERPRET1_R"), 
             data = filter(LTM_dsn, LANGHOMEC1 == "Not selected" & 
                             INTERPRET_R == "Often Needed"))


r_svysummary(by = "INTERPRET_R",
             include = "INTERPRET1_R", 
             data = filter(LTM_dsn, LANGHOMEC1 == "Not selected"))
# FUTURE BIRTH
# RACE PARITY CURRREL AGE4 
r_svysummary(by = "RACE",
             include = "xFUTUREBIRTH", 
             data = filter(LTM_dsn, xFUTUREBIRTH != "I'm not sure"))

# CTJUDGE
r_svysummary(include = c("CTJUDGEC1", "CTJUDGEC2", "CTJUDGEC3", "CTJUDGEC4",
                         'CTJUDGEC5', 'CTJUDGEC6', 'CTJUDGEC7','CTJUDGEC8',
                         'CTJUDGEC9', 'CTJUDGEC10', 'CTJUDGEC11', 'CTJUDGEC12',
                         'CTJUDGEC13','CTJUDGEC14', 'CTJUDGEC15', 
                         'CTJUDGEC16', 'CTJUDGEC17','CTJUDGEC18', 
                         'CTJUDGEC19', 'CTJUDGEC20'))

# Immigration status - CTJUDGEC10 BIRTHCOUNTRYUS == "Any other country"
# Disability - CTJUDGEC12 DISABILITY2 == "Yes"
# Accent - CTJUDGEC8  LANGHOMEC1 == "Not selected" | BIRTHCOUNTRYUS == "Any other country"
# Sexual orientation - CTJUDGEC5 SEXORIENT != "Straight"
# Mental health - CTJUDGEC11 MH2WK_ANXDEP == 1
# Insurance - CTJUDGEC14 INSURANCE == "Medicaid" | INSURC6 == "I had no health insurance for my recent birth"
# Race - CTJUDGEC9 RACE != "White"
# BMI - CTJUDGEC7 BMI4_PREPREG %in% c("Underweight", "Obese+") - CTJUDGEC7
# Marital status - CTJUDGEC4 MARRIED != "Married"
r_svysummary(include = c("CTJUDGEC4"),
             data = filter(LTM_dsn, MARRIED == "Separated/Single"))

r_svysummary(include = c("CTJUDGEC1", "CTJUDGEC2", "CTJUDGEC3", "CTJUDGEC4",
                         'CTJUDGEC5', 'CTJUDGEC6', 
                         'CTJUDGEC11', 
                         'CTJUDGEC13', 'CTJUDGEC15', 
                         'CTJUDGEC16', 'CTJUDGEC17','CTJUDGEC18', 
                         'CTJUDGEC19', 'CTJUDGEC20'),
             data = filter(LTM_dsn, CTJUDGE_ANY == "Some"))

r_svysummary(include = c("CTJUDGEC10"),
             data = filter(LTM_dsn, CTJUDGE_ANY == "Some" & 
                             BIRTHCOUNTRYUS == "Any other country"))

r_svysummary(by = "CTJUDGE_AGE",
             include = "AGE_EXT")

# CTJUDGE_AGE
LTM_final %>% 
  subset(CTJUDGEC19O != "") %>% 
  select(c(CTJUDGEC19O, CTJUDGE_AGE)) %>% 
  View()
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
# GET RID OF PNTA in question
r_svysummary(include = c('FUTUREBABYDIFF1_A1R', 'FUTUREBABYDIFF1_A2R', 
                         'FUTUREBABYDIFF1_A3R', 'FUTUREBABYDIFF1_A4R'), 
             data = filter(LTM_dsn, FUTUREBIRTH != '0'& 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A1R - MIDWIFE
# RACE, INSURANCE, PROVIDER2_R, BIRTHATTEND_R
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A1R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A1R2", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# MIDWIFE CONCERN
r_svysummary(include = c("MIDWIFECONCERNC1", 'MIDWIFECONCERNC2', 
                         'MIDWIFECONCERNC3', 'MIDWIFECONCERNC4', 
                         'MIDWIFECONCERNC5', 'MIDWIFECONCERNC6', 
                         'MIDWIFECONCERNC7'))

# FUTUREBABYDIFF1_A4R - DOULA
# RACE, INSURANCE, RESPONSIBTIER, DOULAANY
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A4R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A4R2", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A2R - Birth center
# RACE, INSURANCE, BIRTHCOUNTRYUS
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A2R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A2R2", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

# FUTUREBABYDIFF1_A3R - Home birth
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A3R", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

r_svysummary(by = "RACE", 
             include = "FUTUREBABYDIFF1_A3R2", 
             data = filter(LTM_dsn, FUTUREBIRTH != '0' & 
                             FUTUREBIRTH != "I'd prefer not to answer"))

#DOULAWANT
r_svysummary(include = c('DOULAWANT_ANY', "DOULAWANT_ALLTHREE",
                         "DOULAWANT_BIRTHONLY"),
             data = filter(LTM_dsn, FUTUREBABYDIFF1_A4 == "I definitely would want this"))

count_svysummary(include = "DOULAWANT_ANY")
# WICBEN 
# CURRREL_R
r_svysummary(by = "DISABILITY", 
             include = "WICBEN")

# WICPP
# RACE INSURANCE PARITY CURRREL_R DISABILITY
r_svysummary(by = "RACE",
             include = "WICPP_ONLY")

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



r_svysummary(include = c(paste0("RESOURCEVALUE_A", 1:12, "R")))
r_svysummary(include = c(paste0("RESOURCEVALUE2_A", 1:10, "R")))

# WICVALUE 949
r_svysummary(include = "WICVALUE_A7", 
             data = filter(LTM_dsn, 
                           !(WICVALUE_A7 %in% c("I did not use this service", 
                                                "I’d prefer not to answer"))))

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
r_svysummary(include = paste0("RESOURCETRUST_A", 1:21),#, "_N"),
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding"))

# COVID
# RACE, INSURANCE, URBANICITY2 DISABILITY
r_svysummary(by = "RACE", 
             include = "ANYCOVID")

# COVIDVAC 
# RACE, INSURANCE, DISABILITY, POLIT2
r_svysummary(by = "RACE", 
             include = c("COVIDVACC_ANY"))

# RACE, INSURANCE, DISABILITY, POLIT2
r_svysummary(#by = "RACE", 
  include = "COVIDVACC2")
# BABYVAC
# RACE INSURANCE PARITY CURRREL_R URBANICITY2 POLIT2
# CHILD_AGE
r_svysummary(by = "CHILD_AGE",
             include = "BABYVAC", 
             data = filter(LTM_dsn, TIME_SINCE_BIRTH_MONTH >= 6))


# CLIMATE
# CENSUS_REG
r_svysummary(#by = "CENSUS_REG", 
  include = paste0("CLIMATEC", 1:9))

r_svysummary(include = paste0("CLIMATEIMPACTC", 1:18),
             data = filter(LTM_dsn, ANY_CLIMATE > 0))

r_svysummary(include = "NEWSIMPACT", 
             data = filter(LTM_dsn, NEWSIMPACT != "I’d prefer not to answer"))
#SNAP 
# RACE INSURANCE PARITY DISABILITY
r_svysummary(by = "RACE", 
             include = c("SNAP_ANY"))




