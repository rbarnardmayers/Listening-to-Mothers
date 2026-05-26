source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# MORB
# MORBPERSIST
# BCONTROL
# BCACQUIRE
# BCOFFERED
# BCPROVIDED
# BCOPTIONS
# CONTRACOER
# CONTRAEXP
# SELFCARE
# CURRREL
# RELSUPPORT
# MH2WK
# RACE, INSURANCE, DISABILITY2
r_svysummary(by = "DISABILITY2", 
             include = "PHQ4_MH2WK_PSYCH")

# BOND
# PPMHSUPP
# RACE, MODE2, INSURANCE, DISABILITY
r_svysummary(by = "DISABILITY",
             include = c("PPMHSUPP"), 
             data = filter(LTM_dsn, MH2WK_ANXDEP == 1))

# COUNSELINT
# COUNSELBARR
# PPMEDS
# Merge Q1 and Q2 MHCOND
# BABYHEALTH
# BABYHEALTHDET
# NICUREAS -- all PNTA
# INFVAC
# BABYCARE



# Flow charts ----
count_svysummary(include = "PREPREG_MHCONDC2")

count_svysummary(by = "PREPREG_MHCONDC2",
                 include = "PHQ4_PREG_ANX")

count_svysummary(by = "PHQ4_PREG_ANX", 
                 include = "PHQ4_PPANX", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC2 == "Not selected"))
count_svysummary(by = "PHQ4_PPANX", 
                 include = "PHQ4_MH2WK_ANX", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC2 != "Not selected" & 
                                 PHQ4_PREG_ANX == "Positive screen for anxiety"))

# PHQ4_PREG_ANX
# PHQ4_PPANX
# PHQ4_MH2WK_ANX
# 
# 

count_svysummary(include = "PREPREG_MHCONDC1")

count_svysummary(by = "PREPREG_MHCONDC1",
                 include = "PHQ4_PREG_DEP")

count_svysummary(by = "PHQ4_PREG_DEP", 
                 include = "PHQ4_PPDEP", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC1 != "Not selected"))
count_svysummary(by = "PHQ4_PPDEP", 
                 include = "PHQ4_MH2WK_DEP", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC1 != "Not selected" & 
                                 PHQ4_PREG_DEP == "Positive screen for depression"))



