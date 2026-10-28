source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# CURREMPLOY
r_svysummary(include = c("CURREMPLOY", "EMPLOYCATPP"))

# PTLEAVE
r_svysummary(by = "MARRIED",
             include = "PTLEAVE", 
             data = filter(LTM_dsn, EMPLOY %in% c("Yes, by someone else", 
                                                  "Yes, self-employed")))

# FOR TEXT -----
r_svysummary(by = "PTLEAVE", 
             include = "WORK_CHANGE")

# MATLEAVE ----
r_svysummary(include = c('FLEXLEAVE', 'SICKLEAVE', 'VACLEAVE', 
                         'STDLEAVE', 'PAIDLEAVETIME', 'PAIDLEAVETOTAL_R'), 
             data = filter(LTM_dsn, PTLEAVE == "Yes"))


r_svysummary(include = c("MATLEAVEC1","MATLEAVEC2","MATLEAVEC3",
                         "MATLEAVEC4","MATLEAVEC5","MATLEAVEC6",
                         "MATLEAVEC7"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes"))

#  & EMPLOY == "Yes, by someone else"

r_svysummary(include = "MATLEAVE_ONLY", 
             data = filter(LTM_dsn, PTLEAVE == "Yes" & 
                             EMPLOY == "Yes, by someone else"))

count_svysummary(include = "MATLEAVEC5", 
             data = filter(LTM_dsn, EMPLOY %in% c("Yes, by someone else",
                                         "Yes, self-employed")))

# Among no mat leave programs
r_svysummary(include = c("MATLEAVE_SUM", "MATLEAVEC1","MATLEAVEC2","MATLEAVEC3",
                         "MATLEAVEC4","MATLEAVEC5","MATLEAVEC6",
                         "MATLEAVEC7"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes" & 
                             EMPLOY == "Yes, by someone else" & 
                             MATLEAVEC5 != "Paid maternity/parental leave program (other than sick, vacation, flex, or short-term disability time)"))

r_svysummary(include = c("MATLEAVE_ONLY", "MATLEAVE_SUM"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes" & 
                             EMPLOY == "Yes, by someone else" & 
                             MATLEAVEC5 != "Paid maternity/parental leave program (other than sick, vacation, flex, or short-term disability time)"))
# Among  mat leave programs
r_svysummary(include = c("MATLEAVE_SUM", "MATLEAVEC1","MATLEAVEC2","MATLEAVEC3",
                         "MATLEAVEC4","MATLEAVEC5","MATLEAVEC6",
                         "MATLEAVEC7"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes" & 
                             EMPLOY == "Yes, by someone else" & 
                             MATLEAVEC5 == "Paid maternity/parental leave program (other than sick, vacation, flex, or short-term disability time)"))

r_svysummary(include = c("MATLEAVE_ONLY"), 
             data = filter(LTM_dsn, PTLEAVE == "Yes" & 
                             EMPLOY == "Yes, by someone else" & 
                             MATLEAVEC5 == "Paid maternity/parental leave program (other than sick, vacation, flex, or short-term disability time)"))

# STDLEAVE1 STDLEAVE1_R
r_svysummary(include = "STDLEAVE1_R", 
             data = filter(LTM_dsn, MATLEAVEC4 != "Not selected"))


# PAIDLEAVEPAY_R
r_svysummary(include = "PAIDLEAVEPAY_R", 
             data = filter(LTM_dsn, MATLEAVEC5 != "Not selected"))

# DUTIES
r_svysummary(include = "DUTIES", 
             data = filter(LTM_dsn, EMPLOY %in% c("Yes, by someone else",
                                                  "Yes, self-employed ") & 
                             Q2FIELD == "Original Fielding"))
r_svysummary(by = "EMPLOY",
             include = "DUTIES", 
             data = filter(LTM_dsn, EMPLOY %in% c("Yes, by someone else",
                                                  "Yes, self-employed ")))
# PARTLEAVE 
r_svysummary(include = "PARTLEAVE_NONE", 
             data = filter(LTM_dsn, 
                           MARRIED %in% c("Married", "Committed Partner")))
r_svysummary(include = "PARTLEAVE", 
             data = filter(LTM_dsn, 
                           MARRIED %in% c("Married", "Committed Partner") & 
                             PARTLEAVE_NONE == "Some time off"))


# PARTEMPLOY
# INSURANCE RACE
r_svysummary(by = "RACE", 
             include = "PARTEMPLOY",
             data = filter(LTM_dsn, MARRIED %in% c("Married", "Committed Partner") & 
                             PARTEMPLOY != "I’d prefer not to answer"))
# INSURANCE RACE
r_svysummary(by = "RACE", 
             include = "PARTCURREMPLOY",
             data = filter(LTM_dsn, CURRREL_R %in% c("Married", "Committed Partner")))

# PARTCURREMPLOY3
# RACE 
r_svysummary(#by = "CURREMPLOY", 
  include = "PARTCURREMPLOY2", 
  data = filter(LTM_dsn, 
                CURRREL_R %in% c("Married", "Committed Partner") & 
                PARTCURREMPLOY %in% c("Yes, by someone else",
                                      "Yes, self-employed") & 
                  PARTCURREMPLOY2 != "I’d prefer not to answer"))

# PARTLEAVETYPE
r_svysummary(#by = 'PARTEMPLOY', 
             include = c("PARTLEAVETYPEC1","PARTLEAVETYPEC2","PARTLEAVETYPEC3",
                         "PARTLEAVETYPEC4","PARTLEAVETYPEC5","PARTLEAVETYPEC6",
                         "PARTLEAVETYPEC7"), 
             data = filter(LTM_dsn, PARTLEAVE1 %in% c("Completely paid", 
                                                      "My spouse or partner used both paid and unpaid leave") & 
                             Q2FIELD != "Standalone" & 
                             PARTCURREMPLOY %in% c("Yes, by someone else", "Yes, self-employed")))


# & PARTEMPLOY == "Yes, by someone else"
r_svysummary(include = "PARTLEAVETYPEC5", 
             data = filter(LTM_dsn, PARTCURREMPLOY%in% c("Yes, by someone else", 
                                                         "Yes, self-employed") & 
                             PARTLEAVE1 %in% c("Completely paid",
                                               "My spouse or partner used both paid and unpaid leave")))

# Among no mat leave programs
r_svysummary(include = c("PARTLEAVE_ONLY", "PARTLEAVE_SUM", 
                         "PARTLEAVETYPEC1","PARTLEAVETYPEC2","PARTLEAVETYPEC3",
                         "PARTLEAVETYPEC4","PARTLEAVETYPEC5","PARTLEAVETYPEC6",
                         "PARTLEAVETYPEC7"), 
             data = filter(LTM_dsn, PARTLEAVE1 %in% c("Completely paid",
                                                      "My spouse or partner used both paid and unpaid leave") & 
                             PARTCURREMPLOY == "Yes, by someone else" &
                             PARTLEAVETYPEC5 != "Paid parental leave (other than sick, vacation, flex, or short-term disability time)"))

# Among  mat leave programs
r_svysummary(include = c("PARTLEAVE_ONLY", "PARTLEAVE_SUM", 
                         "PARTLEAVETYPEC1","PARTLEAVETYPEC2","PARTLEAVETYPEC3",
                         "PARTLEAVETYPEC4","PARTLEAVETYPEC5","PARTLEAVETYPEC6",
                         "PARTLEAVETYPEC7"), 
             data = filter(LTM_dsn, PARTLEAVE1 %in% c("Completely paid",
                                                      "My spouse or partner used both paid and unpaid leave") & 
                             PARTCURREMPLOY == "Yes, by someone else" &
                             PARTLEAVETYPEC5 == "Paid parental leave (other than sick, vacation, flex, or short-term disability time)"))


# MATHOSP PAID LEAVE 
# EMPLOYBEN CURREMPLOY EMPLOY PTLEAVE 
# MATERINTY_LEAVE (selected maternity leave)
# PAIDBENPN
r_svysummary(by = "PTLEAVE",
             include = c("MATHOSP_ANY", "MATHOSPC2", "MATHOSPC3"))

# ELSELEAVE
r_svysummary(include = "ELSELEAVE", 
             data = filter(LTM_dsn, ELSELEAVE != "I’d prefer not to answer"))

r_svysummary(include = "ELSELEAVE1_R",
             data = filter(LTM_dsn, ELSELEAVE  == "Yes"))

r_svysummary(include = "xELSELEAVE2",
             data = filter(LTM_dsn, ELSELEAVE  == "Yes"))

r_svysummary(include = "ELSELEAVE3",
             data = filter(LTM_dsn, ELSELEAVE  == "Yes"))

# IDEALLEAVE
r_svysummary(include = c("IDEALLEAVE_R", "IDEALLEAVE1_R"), 
             data = filter(LTM_dsn, 
                           !is.na(IDEALLEAVE) & 
                             IDEALLEAVE != "I'd prefer not to answer" & 
                             Q2FIELD == "Original Fielding"))

# IDEALLEAVE1
r_svysummary(include = "IDEALLEAVE1_R", 
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding"))

# IDEALLEAVE_xR IDEALLEAVE1_xR
r_svysummary(include = "IDEALLEAVE1_xR", 
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding"))

# CURREMPLOY
# RACE INSURANCE PARITY DISABILITY
r_svysummary(by = "RACE", 
             include = "FTPTWORK_any")

r_svysummary(include = "CURREMPLOY")
r_svysummary(by = "CURREMPLOY", 
             include = "FTPTWORK")

r_svysummary(include = "EMPLOY")
r_svysummary(by = "EMPLOY", 
             include = "CURREMPLOY")

r_svysummary(by = "EMPLOYCAT", 
             include = "CURREMPLOY", 
             data = filter(LTM_dsn, EMPLOY == "No"))
# "Yes, self-employed"
# "Yes, by someone else"
# "No"
r_svysummary(by = "EMPLOYCAT", 
             include = "FTPTWORK", 
             data = filter(LTM_dsn, EMPLOY == "Yes, self-employed"))



# EMPLOYBABYAGE
r_svysummary(include = "xEMPLOYBABYAGE",
             data = filter(LTM_dsn, FTPTWORK_any == "Employed" &  
                             xEMPLOYBABYAGE != "I'd prefer not to answer"))

#MODE1INDEX PHQ4_MH2WK_ANX PHQ4_MH2WK_DEP PHQ4_MH2WK_PSYCH
r_svysummary(#by = "PHQ4_MH2WK_PSYCH",
  include = "EMPLOYBABYAGE_R",
  data = filter(LTM_dsn, FTPTWORK_any == "Employed"))

# RACE, MODE1INDEX, INSURANCE RESPONSIBTIER
r_svysummary(#by = "RESPONSIBTIER",
  include = "EMPLOYBABYAGE_R", 
  data = filter(LTM_dsn, FTPTWORK_any == "Employed"))

r_svysummary(by = "EMPLOYBABYAGE_R" ,
             include = "MODE1INDEX", data = filter(LTM_dsn, EMPLOYCATPP %in% c("Employed full time",
                                                                               "Employed part time")))
# REASONEMPLOY
r_svysummary(include = c("REASONEMPLOYC1","REASONEMPLOYC2","REASONEMPLOYC3",
                         "REASONEMPLOYC4","REASONEMPLOYC5","REASONEMPLOYC6",
                         "REASONEMPLOYC7","REASONEMPLOYC8","REASONEMPLOYC9",
                         "REASONEMPLOYC10"),
             data = filter(LTM_dsn, 
                           CURREMPLOY %in% c("Yes, for someone else",
                                             "Yes, I am self-employed")))
# DEPLETE
r_svysummary(by = "RACE",
             include = "DEPLETE", 
             data = filter(LTM_dsn, WHOEMPLOY == "The same employer you worked for during your recent pregnancy" & 
                             DEPLETE %in% c("Yes, I had some paid time off", "No, I had used up all my paid time off")))


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
r_svysummary(include = c("EMPLOYLACT"),
             data = filter(LTM_dsn, CURREMPLOY == "Yes, for someone else"))

r_svysummary(include = "EMPLOYLACT1",
             data = filter(LTM_dsn, CURREMPLOY == "Yes, for someone else" & 
                             EMPLOYLACT == "Yes, I needed support for lactation or breastfeeding"))

r_svysummary(include = "EMPLOYLACT2",
             data = filter(LTM_dsn, 
                           EMPLOYLACT == "Yes, I needed support for lactation or breastfeeding"))

# FTPTWORK INSURANCE URBANICITY2 DISABILITY2 PTLEAVE
r_svysummary(by = "PTLEAVE",
             include = "EMPLOYLACT2_R",
             data = filter(LTM_dsn, 
                           EMPLOYLACT == "Yes, I needed support for lactation or breastfeeding"))


# WORKWHILECARE
r_svysummary(include = "WORKWHILECARE", 
             data = filter(LTM_dsn, CURREMPLOY %in% c("Yes, for someone else",
                                                      "Yes, I am self-employed")))

# CHALLENGEWORK
# CHALLENGEWORK_A1
r_svysummary(include = c("CHALLENGEWORK_A1", "CHALLENGEWORK_A2", "CHALLENGEWORK_A3", 
                         "CHALLENGEWORK_A4", "CHALLENGEWORK_A5", "CHALLENGEWORK_A6", 
                         "CHALLENGEWORK_A7", "CHALLENGEWORK_A8"),
             data = filter(LTM_dsn, 
                           Q2FIELD == "Original Fielding" & CURREMPLOY %in% c("Yes, for someone else",
                                                                              "Yes, I am self-employed")))
# EMPLOYINTENT
# PARITY POLIT2
r_svysummary(by = "POLIT2", 
             include = "EMPLOYINTENT_R",
             data = filter(LTM_dsn, CURREMPLOY == "No"))

