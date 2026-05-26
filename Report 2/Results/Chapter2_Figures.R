source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# INTERVAL
# BIRTHVIEW
# PRESSURE
# DECLINEDDETAIL
# CSECTFEEL
r_svysummary(include = c("CSECTFEEL_POS", "CSECTFEEL_NEG"),
             data = filter(LTM_dsn, MODE1INDEX == "Cesarean all"))

# TRUST
# MISTRUST
# SAFETY
# HOSPSAFE
# LACKSAFE
# SAFEBABY
# MISTREAT
# RACE, INSURANCE, BIRTHATTEND, AGE
r_svysummary(by = "BIRTHATTEND_R2",
             include = "ANYMISTREAT")

# PERCEIVEDRE
# PERCEIVEDID

