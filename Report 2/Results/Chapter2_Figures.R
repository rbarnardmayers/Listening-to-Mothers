source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# INTERVAL
# BIRTHVIEW
# PRESSURE
# RACE, INSURANCE, PRIOR_C, PARITY, DISABILITY2
r_svysummary(by = "PARITY", 
             include = "PRESSURE_A5")

r_svysummary(by = "CSECTIONTYPE", 
             include = "PRESSURE_A5")

r_svysummary(by = "MODE2INDEX", 
             include = "PRESSURE_A5", 
             data = filter(LTM_dsn, PARITY == "Nulliparous"))


# DECLINEDDETAIL
# CSECTFEEL
r_svysummary(include = c("CSECTFEEL_POS", "CSECTFEEL_NEG"),
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all"))

# TRUST
# MISTRUST
r_svysummary(include = c("MISTRUSTC1", 'MISTRUSTC2', 'MISTRUSTC3', 'MISTRUSTC4', 
                         'MISTRUSTC5', 'MISTRUSTC6', 'MISTRUSTC7', 'MISTRUSTC8'))

# SAFETY
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
# MISTREAT
# RACE, INSURANCE, BIRTHATTEND, AGE
r_svysummary(by = "BIRTHATTEND_R2",
             include = "ANYMISTREAT")

raw_svysummary(include = c('MISTREATC1', 'MISTREATC2', 'MISTREATC3', 
                         'MISTREATC4', 'MISTREATC5', 'MISTREATC6', 
                         'MISTREATC7', "MISTREATC8"))

# PERCEIVEDRE

# Postpartum Weight 




# PERCEIVEDID

