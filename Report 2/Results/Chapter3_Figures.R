source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# MORB table ----
r_svysummary(include = c("MORB_A1", "MORB_A2"), 
                 data = filter(LTM_dsn, 
                               MODE1INDEX == "Vaginal all"))

r_svysummary(include = c("MORB_A3", "MORB_A4"), 
                 data = filter(LTM_dsn,
                               MODE1INDEX == "Cesarean all"))

r_svysummary(include = c(paste0("MORB_A", 5:20)))

r_svysummary(include = c("MORB_A9"), 
                 data = filter(LTM_dsn, 
                               FEED1WEEKC1 == "Breast milk"))
# MORBPERSIST ----
r_svysummary(include = c("MORBPERSIST_A1"), 
             data = filter(LTM_dsn, 
                           MORB_A1 %in% c("A major new problem", 
                                          "A minor new problem")))


# BCONTROL ----
# BCACQUIRE
# BCOFFERED
r_svysummary(include = c("BCOFFEREDC1", "BCOFFEREDC2", "BCOFFEREDC3", 
                         "BCOFFEREDC4", "BCOFFEREDC5"), 
             data = filter(LTM_dsn, BCACQUIRE == "Yes, I needed a health care provider to get the birth control I wanted"))

# BCPROVIDED
# BCOPTIONS
# CONTRACOER
# CONTRAEXP
# SELFCARE
r_svysummary(by = "PARITY", 
             include = c('SELFCARE_A1', 'SELFCARE_A2', 'SELFCARE_A3',
                         'SELFCARE_A4', 'SELFCARE_A5', 'SELFCARE_A6'))

# CURRREL
r_svysummary(by = "MARRIED", 
             include = "CURRREL")

# RELSUPPORT
# MH2WK
# RACE, INSURANCE, DISABILITY2
r_svysummary(by = "DISABILITY2", 
             include = "PHQ4_MH2WK_PSYCH")

# BOND

# SUPPORT
r_svysummary(include = c("FAMFRISUPP_A1", 'FAMFRISUPP_A2',
                         'FAMFRISUPP_A3', 'FAMFRISUPP_A4'))

r_svysummary(include = c("MAXSUPP_A1", 'MAXSUPP_A2',
                         'MAXSUPP_A3', 'MAXSUPP_A4'))

# PPMHSUPP
# RACE, MODE1INDEX, INSURANCE, DISABILITY
r_svysummary(by = "MODE1INDEX",
             include = c("PP_UNMET_NEEDS"), 
             data = filter(LTM_dsn, MH2WK_ANXDEP == 1))

# COUNSELINT
# COUNSELBARR
r_svysummary(include = c("COUNSELBARRC1", 'COUNSELBARRC2', 'COUNSELBARRC3', 
                         'COUNSELBARRC4', 'COUNSELBARRC5', 'COUNSELBARRC6', 
                         'COUNSELBARRC7', 'COUNSELBARRC8'),
             data = filter(LTM_dsn, 
                           COUNSELINT == "Yes"))

# PPMEDS
# RACE, INSURANCE, URBANICITY2, DISABILITY, PARITY, MODE1INDEX
r_svysummary(by = "AGE4",
             include = c("PP_SUPP_ONLY"),
             data = filter(LTM_dsn, 
                           MH2WK_ANXDEP == 1))

r_svysummary(by = "AGE4",
             include = c("PP_ANYSUPP"),
             data = filter(LTM_dsn, 
                           MH2WK_ANXDEP == 1))

# BABYHEALTH
# BABYHEALTHDET
# NICUREAS -- all PNTA
# INFVAC
# BABYCARE
# MH2WKMHCOND
r_svysummary(by = "RACE",
             include = "MH2WK_ANXDEP")

r_svysummary(include = c('PREPREG_MHCONDC2', 
                         'PHQ4_PREG_ANX', 
                         'PHQ4_PPANX'), 
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield") & 
                           PHQ4_MH2WK_ANX == "Positive screen for anxiety"))



r_svysummary(include = c('PREPREG_MHCONDC1', 
                         'PHQ4_PREG_DEP', 
                         'PHQ4_PPDEP'), 
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield") & 
                             PHQ4_MH2WK_DEP == "Positive screen for depression"))



