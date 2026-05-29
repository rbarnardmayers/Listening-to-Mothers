source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

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

# PPDISTANCE
r_svysummary(include = "PPDISTANCE")


# INTENDLOCALE_R
# RACE, INSURANCE, PARITY, DISABILITY2, URBANICITY2
r_svysummary(by = "URBANICITY2", 
             include = "INTENDLOCALE_R")


# COSTMOM
r_svysummary(by = "xCOSTMOM", 
             include = "xCOSTMOM1")

# COSTBABY
r_svysummary(by = "xCOSTBABY", 
             include = "xCOSTBABY1")


