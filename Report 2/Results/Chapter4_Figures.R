source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# PTLEAVE
# MATLEAVE
r_svysummary(include = c("MATLEAVEC1","MATLEAVEC2","MATLEAVEC3",
                         "MATLEAVEC4","MATLEAVEC5","MATLEAVEC6",
                         "MATLEAVEC7"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes"))

# STDLEAVE1
r_svysummary(include = "xSTDLEAVE1", 
             data = filter(LTM_dsn, MATLEAVEC4 != "Not selected"))

# PAIDLEAVEPAY
r_svysummary(include = "xPAIDLEAVEPAY", 
             data = filter(LTM_dsn, MATLEAVEC5 != "Not selected"))

# DUTIES
r_svysummary(include = "DUTIES", 
             data = filter(LTM_dsn, EMPLOY %in% c("Yes, by someone else",
                                                  "Yes, self-employed ")))
# PARTEMPLOY
# PARTEMPLOY1

# PARTLEAVETYPE
r_svysummary(include = c("PARTLEAVETYPEC1","PARTLEAVETYPEC2","PARTLEAVETYPEC3",
                         "PARTLEAVETYPEC4","PARTLEAVETYPEC5","PARTLEAVETYPEC6",
                         "PARTLEAVETYPEC7"), 
             data = filter(LTM_dsn, PARTLEAVE1 %in% c("Completely paid",
                                                      "My spouse or partner used both paid and unpaid leave")))
# IDEALLEAVE
r_svysummary(include = "IDEALLEAVE", 
             data = filter(LTM_dsn, 
                           !is.na(IDEALLEAVE) & 
                             IDEALLEAVE != "I'd prefer not to answer"))

# IDEALLEAVE1
r_svysummary(include = "IDEALLEAVE1", 
             data = filter(LTM_dsn, 
                           !is.na(IDEALLEAVE1) & 
                             IDEALLEAVE1 != "I'd prefer not to answer"))


# CURREMPLOY
# EMPLOYBABYAGE
# RACE, MODE1INDEX, INSURANCE
r_svysummary(by = "INSURANCE",
             include = "EMPLOYBABYAGE_R", 
             data = filter(LTM_dsn, 
                           CURREMPLOY %in% c("Yes, for someone else",
                                             "Yes, I am self-employed")))

# REASONEMPLOY
r_svysummary(include = c("REASONEMPLOYC1","REASONEMPLOYC2","REASONEMPLOYC3",
                         "REASONEMPLOYC4","REASONEMPLOYC5","REASONEMPLOYC6",
                         "REASONEMPLOYC7","REASONEMPLOYC8","REASONEMPLOYC9",
                         "REASONEMPLOYC10"),
             data = filter(LTM_dsn, 
                           CURREMPLOY %in% c("Yes, for someone else",
                                             "Yes, I am self-employed")))
# TIMEOFF
# CURRBEN
r_svysummary(include = c("xCURRBEN_A1"),
             data = filter(LTM_dsn, 
                           xCURRBEN_A1 != "I'd prefer not to answer" & 
                             CURREMPLOY %in% c("Yes, for someone else")))

r_svysummary(include = c("xCURRBEN_A2"),
             data = filter(LTM_dsn, 
                           xCURRBEN_A2 != "I'd prefer not to answer" & 
                             CURREMPLOY %in% c("Yes, for someone else")))

r_svysummary(include = c("xCURRBEN_A3"),
             data = filter(LTM_dsn, 
                           xCURRBEN_A3 != "I'd prefer not to answer" & 
                             CURREMPLOY %in% c("Yes, for someone else")))

r_svysummary(include = c("xCURRBEN_A4"),
             data = filter(LTM_dsn, 
                           xCURRBEN_A4 != "I'd prefer not to answer" & 
                             CURREMPLOY %in% c("Yes, for someone else")))

# EMPLOYLACT
r_svysummary(include = c("EMPLOYLACT"))
r_svysummary(by = "EMPLOYLACT",
             include = "EMPLOYLACT1")

r_svysummary(include = "EMPLOYLACT2",
             data = filter(LTM_dsn, 
                           EMPLOYLACT == "Yes, I needed support for lactation or breastfeeding"))

# CHALLENGEWORK
# CHALLENGEWORK_A1
r_svysummary(include = c("CHALLENGEWORK_A1", "CHALLENGEWORK_A2", "CHALLENGEWORK_A3", 
                         "CHALLENGEWORK_A4", "CHALLENGEWORK_A5", "CHALLENGEWORK_A6", 
                         "CHALLENGEWORK_A7", "CHALLENGEWORK_A8"),
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding" & CURREMPLOY %in% c("Yes, for someone else",
                                             "Yes, I am self-employed")))
# NOTEMPLOY
# PARTCURREMPLOY3
# RACE
r_svysummary(by = "RACE", 
             include = "PARTCURREMPLOY3", 
             data = filter(LTM_dsn, 
                           PARTCURREMPLOY %in% c("Yes, by someone else",
                                                 "Yes, self-employed")))
# CAREGIVE
