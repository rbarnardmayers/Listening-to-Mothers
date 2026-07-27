source("~/Desktop/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# INTERVAL
table(LTM_final$INTERVAL)

r_svysummary(include = c("R_INTERVAL", "INTERVAL"), 
             data = filter(LTM_dsn, EMPLOY == "Yes, by someone else"))

# RACE INSURANCE DISABILITY RESPONSIBTIER

r_svysummary(by = "RESPONSIBTIER", 
             include = c("INTERVAL"), 
             data = filter(LTM_dsn, EMPLOY == "Yes, by someone else"))

# up until a week (1 - 7), later (8+)
r_svysummary(by = "RESPONSIBTIER", 
             include = c("INTERVAL_7days"), 
             data = filter(LTM_dsn, EMPLOY == "Yes, by someone else"))

r_svysummary(include = "INTERVAL0", 
             data = filter(LTM_dsn, EMPLOY == "Yes, by someone else"))

# BIRTHVIEW
# PRESSURE 
r_svysummary(include = c("PRESSURE_A1", 'PRESSURE_A2', 'PRESSURE_A3', 'PRESSURE_A4', 
                         'PRESSURE_A5', 'PRESSURE_A6'),
             data = filter(LTM_dsn, UNPLANNED_SCREEN == "Unplanned"))

# Induction pressure 
# MEDINDUCE MODE1INDEX CSECTIONTYPE
r_svysummary(by = "CSECTIONTYPE", 
             include = "PRESSURE_A1",
             data = filter(LTM_dsn, UNPLANNED_SCREEN == "Unplanned"))

r_svysummary(by = "PRESSURE_A1", 
             include = "UNPLANNED_VAG",
             data = filter(LTM_dsn, UNPLANNED_SCREEN == "Unplanned"))

r_svysummary(include = "MEDINDUCE", 
             data = filter(LTM_dsn, UNPLANNED_SCREEN == "Unplanned"))
# PLANNEDFEEDC1 PLANNEDFEED PLANNEDFEED_ONLY
r_svysummary(by = "PLANNEDFEED_ONLY", 
             include = "PRESSURE_A6")

# RACE, INSURANCE, PRIOR_C, PARITY, DISABILITY2
r_svysummary(by = "PARITY", 
             include = "PRESSURE_A5")

r_svysummary(by = "MODE2INDEX", 
             include = "PRESSURE_A5")

r_svysummary(by = "MODE2INDEX", 
             include = "PRESSURE_A5", 
             data = filter(LTM_dsn, PARITY == "Nulliparous"))


# CSECTFEEL PRESSURE_A5
r_svysummary(#by = "PRESSURE_A5", 
             include = c("CSECTFEEL_POS", "CSECTFEEL_NEG", 'CSECTFEEL_ALL'), 
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all"))
LTM_final %>% 
  subset(CSECTFEELC10O != "") %>%
  select(c(CSECTFEEL_ALL, CSECTFEEL_POS, CSECTFEEL_NEG, CSECTFEELC10, CSECTFEELC10O)) %>% 
  View()

# MODE2INDEX 
r_svysummary(by = "MODE2INDEX", 
             include = c("CSECTFEEL_POS", "CSECTFEEL_NEG", 'CSECTFEEL_ALL'), 
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all" & 
                             PARITY == "Multiparous"))
# BIRTHVIEW_R CSECTIONTYPE PRESSURE_A5 MODE2INDEX
r_svysummary(#by = "MODE2INDEX", 
             include = c("CSECTFEEL_POS", "CSECTFEEL_NEG"),
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all"))

r_svysummary(by = "BIRTHVIEW_R", 
             include = c("CSECTFEEL_ALL"),
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all"))

# 306,638.9787

# TRUSTTEAM
# RACE INSURANCE BIRTHATTEND_2 ENGPROF DISABILITY
r_svysummary(by = "DISABILITY", 
             include = "TRUSTTEAM_NONE")


# MISTRUST
r_svysummary(include = c("MISTRUSTC1", 'MISTRUSTC2', 'MISTRUSTC3', 'MISTRUSTC4', 
                         'MISTRUSTC5', 'MISTRUSTC6', 'MISTRUSTC7', 'MISTRUSTC8'))

# SAFETY
# RACE INSURANCE MODE1INDEX DISABILITY RESPONSIBTIER
r_svysummary(by = "RACE", 
             include = "NOSAFETY")
r_svysummary(by = "RACE", 
             include = "NOSAFETY2")

# HOSPSAFE
r_svysummary(include = c('HOSPSAFEC1', 'HOSPSAFEC2', 'HOSPSAFEC3', 'HOSPSAFEC4', 
                         'HOSPSAFEC5', 'HOSPSAFEC6', 'HOSPSAFEC7', 'HOSPSAFEC8', 
                         'HOSPSAFEC9', 'HOSPSAFEC10', 'HOSPSAFEC11', 'HOSPSAFEC12',
                         'HOSPSAFEC13', 'HOSPSAFEC14', 'HOSPSAFEC15', 'HOSPSAFEC16'),
            data = filter(LTM_dsn, SAFETY %in% c("Always safe", "Usually safe",
                                                 "Sometimes safe", "Rarely safe")))
# LACKSAFE
r_svysummary(include = c('LACKSAFEC1', 'LACKSAFEC2', 'LACKSAFEC3', 'LACKSAFEC4', 
                         'LACKSAFEC5', 'LACKSAFEC6', 'LACKSAFEC7', 'LACKSAFEC8', 
                         'LACKSAFEC9', 'LACKSAFEC10', 'LACKSAFEC11', 'LACKSAFEC12',
                         'LACKSAFEC13', 'LACKSAFEC14', 'LACKSAFEC15', 'LACKSAFEC16'),
             data = filter(LTM_dsn, SAFETY %in% c("Usually safe", "Sometimes safe", 
                                                  "Rarely safe", "Never safe")))


# SAFEBABY
# NOSAFEBABY
# RACE DISABILITY 
r_svysummary(by = "DISABILITY", 
             include = "NOSAFEBABY")

# MISTREAT
# RACE, INSURANCE, BIRTHATTEND, AGE DISABILITY
r_svysummary(by = "DISABILITY",
             include = "ANYMISTREAT")

# Type of mistreat
count_svysummary(include = c('MISTREATC1', 'MISTREATC2', 'MISTREATC3', 
                         'MISTREATC4', 'MISTREATC5', 'MISTREATC6', 
                         'MISTREATC7', "MISTREATC8"))

count_svysummary(include = c('THREATC3', "SHOUTC3", 'SHAREPRIVC3',
                             'FORCEDC3', 'WHOPRIVC3', 'WITHHELDC3',
                             'IGNOREC3', 'PABUSEC3'))

# By provider type
r_svysummary(include = "ANYMISTREAT_MW", 
             data = filter(LTM_dsn, BIRTHATTEND2 == "All Doctors"))
# BIRTHATTEND_R
r_svysummary(by = "BIRTHATTEND2",
             include = c("ANYMISTREAT_OB", "ANYMISTREAT_MW", "ANYMISTREAT_N"))

# INTENDLOCALE
r_svysummary(include = "INTENDLOCALE", 
             data = filter(LTM_dsn, INTENDLOCALE != "I’d prefer not to answer" & 
                             INTENDLOCALE != "Yes"))



