source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# Table 2.1: PERCEIVEDRACE
r_svysummary(include = "RACE")
r_svysummary(by = "RACE", 
             include = c('PERCEIVEDREC1', 'PERCEIVEDREC2', 'PERCEIVEDREC3',
                         'PERCEIVEDREC4_6', 'PERCEIVEDREC7'))

r_svysummary(include = "BLACKWHITE")
r_svysummary(by = "BLACKWHITE", 
             include = c('PERCEIVEDREC1', 'PERCEIVEDREC2', 'PERCEIVEDREC3',
                         'PERCEIVEDREC4_6', 'PERCEIVEDREC7'))

r_svysummary(include = "AFROLATINE")
r_svysummary(by = "AFROLATINE", 
             include = c('PERCEIVEDREC1', 'PERCEIVEDREC2', 'PERCEIVEDREC3',
                         'PERCEIVEDREC4_6', 'PERCEIVEDREC7'))

r_svysummary(include = "AIANWHITE")
r_svysummary(by = "AIANWHITE", 
             include = c('PERCEIVEDREC1', 'PERCEIVEDREC2', 'PERCEIVEDREC3',
                         'PERCEIVEDREC4_6', 'PERCEIVEDREC7'))
# Table 3.1: MORB
# data = filter(LTM_dsn, 
#               MORB_A25 %in% c("A major new problem", 
#                               "A minor new problem") & 
#                 TIME_SINCE_BIRTH >= 24)

r_svysummary(include = c("MORB_A1", "MORB_A2"), 
             data = filter(LTM_dsn, 
                           MODE1INDEX == "Vaginal all"))

r_svysummary(include = c("MORB_A3", "MORB_A4"), 
             data = filter(LTM_dsn,
                           MODE1INDEX == "Cesarean all"))

r_svysummary(include = c(paste0("MORB_A", 5:20)))

r_svysummary(include = c('MORB_A7', 'MORB_A8', "MORB_A9"), 
             data = filter(LTM_dsn, 
                           FEED1WEEKC1 == "Breast milk"))


r_svysummary(include = "MORB_A1", 
             data = filter(LTM_dsn, 
                           MORB_A1 %in% c("A major new problem", 
                                           "A minor new problem") & 
                             TIME_SINCE_BIRTH >= 24))

# Table 3.2: RELCHANGE


# Table 5.COSTMOM
r_svysummary(by = "xCOSTMOM",
             include = "xCOSTMOM1")

# Table 5.COSTBABY
r_svysummary(include = "xCOSTBABY")
r_svysummary(by = "xCOSTBABY",
             include = "xCOSTBABY1")


# Table AVGGOOPBYINSUR
# INSURC2 - medicaid
# INSURC1 - private
# INSURC6 - none
# INSURANCE
r_svysummary(by = "INSURC6",
             include = c("COSTMOM1"))
# c(BILLED_MOM_0 PAID_MOM_0 BILLED_BABY_0 PAID_BABY_0)
r_svysummary(#by = "INSURANCE",
             include = c('BILLED_MOM_0', 'PAID_MOM_0'))

# INSURBABYC2 - medicaid
# INSURBABYC1 - private
# INSURBABYC5 - none
# INSURBABY
r_svysummary(by = "INSURBABY",
             include = c("COSTBABY1"))

r_svysummary(#by = "INSURBABYC5",
             include = c('BILLED_BABY_0', 'PAID_BABY_0'))
