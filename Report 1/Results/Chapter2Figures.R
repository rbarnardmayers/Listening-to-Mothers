source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")

# Figure 2.1 
# RACE, INSURANCE, PARITY
r_svysummary(by = "INSURANCE", 
             include = c("PREPREG_PHYSCONDC1", "PREPREG_PHYSCONDC2"))

# Figure 2.2
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = c("PREPREG_MHCONDC1", "PREPREG_MHCONDC2", 
                         "PREPREG_MHCONDC3", "PREPREG_MHCONDC4"))

# Figure 2.3 
# RACE, INSURANCE, DISABILITY
r_svysummary(by = "RACE", 
             include = "FIRSTVISIT", 
             data = filter(LTM_dsn, !is.na(LEARNED2)))

# Figure 2.4 
r_svysummary(include = c("NOPRENATALC1", "NOPRENATALC2", "NOPRENATALC3",
                         "NOPRENATALC4", "NOPRENATALC5", "NOPRENATALC6", 
                         "NOPRENATALC7", "NOPRENATALC8", "NOPRENATALC9",
                         "NOPRENATALC10","NOPRENATALC11","NOPRENATALC12"), 
             data = filter(LTM_dsn, !is.na(LEARNED2)))

# Figure 2.5
# Right now doctor, midiwfe, other.
# Asking if want OB, Midwife, other
# RACE, INSURANCE
r_svysummary(include = "PROVIDER2", 
             data = filter(LTM_dsn, !is.na(LEARNED2)))

# Figure 2.6
# RACE, INSURANCE
r_svysummary(include = "PROVIDERCHOICE", 
             data = filter(LTM_dsn, !is.na(LEARNED2)))


# Figure 2.7
r_svysummary(include = c("CARESETTINGC1", "CARESETTINGC2","CARESETTINGC3",
                         "CARESETTINGC4","CARESETTINGC5","CARESETTINGC6",
                         "CARESETTINGC7"), 
             data = filter(LTM_dsn, !is.na(LEARNED2)))

# Figure 2.8
# RACE, INSURANCE
r_svysummary(by = "RACE", 
             include = "CARETYPE1",
              data = filter(LTM_dsn, !is.na(LEARNED2)))

# Figure 2.9 
# RACE, INSURANCE, URBANICITY
r_svysummary(by = "RACE",
             include = "CARETYPEPREF",
             data = filter(LTM_dsn, CARETYPEC2 == 1))

# Figure 2.10 
r_svysummary(include = c("WHYTELEC1", "WHYTELEC2", "WHYTELEC3", 
                         "WHYTELEC4", "WHYTELEC5", "WHYTELEC6", 
                         "WHYTELEC7"),
             data = filter(LTM_dsn, CAREMODEC2 == "Televisits"))

# Figure 2.11
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "CAREMODEPREF")

# Figure 2.12
# RACE, INSURANCE, PROVIDER
r_svysummary(by = "RACE", 
             include = "DOULAC1")

# Figure 2.13
r_svysummary(include = c("DOULA1C1", "DOULA1C2", "DOULA1C3", 
                         "DOULA1C4", "DOULA1C5"), 
             data = filter(LTM_dsn, DOULAC1 == "Yes, during my pregnancy"))

# Figure 2.14
# INSURANCE, URBANICITY, PARITY, POLIT2
r_svysummary(by = "RACE",
             include = "CURREDUC")

# Figure 2.15
r_svysummary(include = c("EDUCIMPACTC1", "EDUCIMPACTC2", "EDUCIMPACTC3",
                         "EDUCIMPACTC4", "EDUCIMPACTC5","EDUCIMPACTC6"), 
             data = filter(LTM_dsn, CURREDUC == "Yes"))

# Figure 2.16
r_svysummary(include = c('PREGCONDITIONC1','PREGCONDITIONC2','PREGCONDITIONC3',
                         'PREGCONDITIONC4','PREGCONDITIONC5','PREGCONDITIONC6',
                         'PREGCONDITIONC7','PREGCONDITIONC8','PREGCONDITIONC9',
                         'PREGCONDITIONC10'))

# Figure 2.17
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "PHQ4_PREG_DEP")

# Figure 2.18
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "PHQ4_PREG_ANX")

# Figure 2.19
#RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "MSUPPORT_ONLY", 
             data = filter(LTM_dsn, PHQ4_PREG_DEP == "Positive screen for depression" | 
                             PHQ4_PREG_ANX == "Positive screen for anxiety"))

# Figure 2.20
# RACE, INSURANCE, URBANICITY2, DISABILITY
# Neither row only
r_svysummary(by = "RACE", 
             include = "MSUPPORT_ONLY", 
             data = filter(LTM_dsn, PHQ4_PREG_DEP == "Positive screen for depression" | 
                             PHQ4_PREG_ANX == "Positive screen for anxiety"))

# Figure 2.21
r_svysummary(include = c('SOCIALNEEDC1', 'SOCIALNEEDC2', 'SOCIALNEEDC3', 
                         'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                         'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                         'SOCIALNEEDC10'))

# Figure 2.22
# RACE, URBANICITY2, DISABILITY, MARRIED, INCCAT2
r_svysummary(by = "URBANICITY2", 
             include = "SUM_SOCIALNEED")

# Figure 2.23
# RACE, INSURANCE, URBANICITY2, INCCAT2
r_svysummary(by = "URBANICITY2", 
             include = "EMPLOYBEN", 
             data = filter(LTM_dsn, EMPLOYEDBY == "Employed by someone else"))

# Figure 2.24
# RACE, INSURANCE, INCCAT2
r_svysummary(by = "RACE", 
             include = "EMPLOYCHANGE1", 
             data = filter(LTM_dsn, EMPLOYCHANGE == "Yes, I needed some temporary workplace changes for a healthy pregnancy"))

# Figure 2.25
r_svysummary(by = "BIGBABY1", 
             include = "BW_CAT")

# Figure 2.26
# FLOWCHART
r_svysummary(include = "BIGBABY1", 
             data = filter(LTM_dsn, PRIOR_C == 0 & 
                             xGESTAGE %in% c("Early Term: 37 to 38 weeks", 
                                             "Full Term: 39 to 40 weeks", 
                                             "Late Term: 41 weeks")))
r_svysummary(by = "BIGBABY1", 
             include = "BIGBABY2",
             data = filter(LTM_dsn, PRIOR_C == 0 & 
                             xGESTAGE %in% c("Early Term: 37 to 38 weeks", 
                                             "Full Term: 39 to 40 weeks", 
                                             "Late Term: 41 weeks")))

r_svysummary(by = "BIGBABY2",
             include = c("MEDINDUCE", "MODE2023"),
             data = filter(LTM_dsn, PRIOR_C == 0 & 
                             xGESTAGE %in% c("Early Term: 37 to 38 weeks", 
                                             "Full Term: 39 to 40 weeks", 
                                             "Late Term: 41 weeks")))

# Figure 2.27
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = c("PLANNEDFEED_ONLY"))

# Figure 2.28
r_svysummary(by = "BMI4_PREPREG", 
             include = "WEIGHT_REC_D")



