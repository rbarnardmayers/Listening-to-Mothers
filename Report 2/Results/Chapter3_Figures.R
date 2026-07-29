source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Desktop/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# MORB table ----

# MORBPERSIST ----
r_svysummary(include = c("MORBPERSIST_A25"), 
             data = filter(LTM_dsn, 
                           MORB_A25 %in% c("A major new problem", 
                                          "A minor new problem") & 
                             TIME_SINCE_BIRTH >= 24))

# BASES 
r_svysummary(include = "MORBPERSIST_A22")
LTM_final %>% 
  filter(MORB_A22 %in% c("A major new problem", 
                                "A minor new problem") & 
           TIME_SINCE_BIRTH >= 24) %>% 
  nrow()

# WEIGHT GAIN/LOSS ----
# WEIGHTGAIN_R is pre-preg to pregnancy 
# WEIGHTLOSS_Q1 is pregnancy to time of Q1
# WEIGHTLOSS_Q2 is Q1 to Q2

r_svysummary(#by = "PARITY", 
             include = c("WEIGHTGAIN_R", 
                         "WEIGHTLOSS_Q2"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             !is.na(WEIGHTLOSS_Q2)))

r_svysummary(by = "TIME_SINCE_BIRTH_QUART", 
             include = c("WEIGHTLOSS_Q2R"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             !is.na(WEIGHTLOSS_Q2)))

r_svysummary(by = "TIME_SINCE_BIRTH_HALF", 
             include = c("WEIGHTLOSS_Q2R"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             !is.na(WEIGHTLOSS_Q2)))

r_svysummary(by = "TIME_SINCE_BIRTH_QUART", 
             include = c("WEIGHTLOSS_Q2"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             PARITY == "Nulliparous" & 
                             !is.na(WEIGHTLOSS_Q2)))

r_svysummary(by = "TIME_SINCE_BIRTH_QUART", 
             include = c("WEIGHTLOSS_Q2"), 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             PARITY == "Multiparous" & 
                             !is.na(WEIGHTLOSS_Q2)))

r_svysummary(include = "TIME_SINCE_BIRTH",
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                             !is.na(WEIGHTLOSS_Q2)))

# MATHOSP ----
# RACE INSURANCE AGE4 DISABILITY 
r_svysummary(by = "EMPLOYBEN",
             include = c("MATHOSP_ANY", "MATHOSPC2", "MATHOSPC3"))

r_svysummary(include = c("PTLEAVE"))

# MATER
# RACE INSURANCE AGE4 DISABILITY MODE1INDEX URBANICITY2
r_svysummary(by = "URBANICITY2",
             include = "MATER_ANY")

# BABYHOSP
# RACE INSURANCE AGE4 DISABILITY
r_svysummary(by = "RACE",
             include = "BABYHOSP2", 
             data = filter(LTM_dsn, 
                           BABYHOSP2 != "I’d prefer not to answer"))

# BABYER
# RACE INSURANCE AGE4 DISABILITY
r_svysummary(by = "RACE",
             include = "BABYER", 
             data = filter(LTM_dsn, 
                           BABYER != "I’d prefer not to answer"))

# BCONTROL 
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
# MARRIED RACE INSURANCE
r_svysummary(#by = "INSURANCE", 
  include = "CURRREL_R")

# RELSUPPORT
# MH2WK
# RACE, INSURANCE, DISABILITY2
r_svysummary(by = "DISABILITY2", 
             include = "PHQ4_MH2WK_PSYCH")

# BOND
# RACE INSURANCE DISABILITY
r_svysummary(by = "DISABILITY",
             include = "BOND_R")

# PROBABLEPTSD PHQ4_MH2WK_PSYCH PHQ4_MH2WK_ANX PHQ4_MH2WK_DEP
r_svysummary(by = "PROBABLEPTSD",
             include = "BOND_R")


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
r_svysummary(include = c("TIME_SINCE_BIRTH"), 
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
r_svysummary(by = "RACE",
             include = c("PP_SUPP_ONLY"),
             data = filter(LTM_dsn, 
                           MH2WK_ANXDEP == 1))

r_svysummary(by = "AGE4",
             include = c("PP_ANYSUPP"),
             data = filter(LTM_dsn, 
                           MH2WK_ANXDEP == 1))
r_svysummary(by = "PP_ANYSUPP",
             include = c("PHQ4_MH2WK_DEP"))
r_svysummary(by = "PP_ANYSUPP",
             include = c("PHQ4_MH2WK_ANX"))

# TRAUMA 
# RACE INSURANCE MODE1INDEX BIRTHATTEND_R DISABILITY RESPONSIBTIER
r_svysummary(by = "RESPONSIBTIER",
             include = "MATTRAUMA")

# BABYHEALTH
# BABYHEALTHDET
# NICUREAS -- all PNTA
# INFVAC
# BABYCARETIME
r_svysummary(include = c("xBABYCARETIME_B0", 'xBABYCARETIME_B1', 'xBABYCARETIME_B2',
                         'xBABYCARETIME_B3', 'xBABYCARETIME_B4', 'xBABYCARETIME_B5', 
                         'xBABYCARETIME_B6', 'xBABYCARETIME_B7', 'xBABYCARETIME_B8',
                         'xBABYCARETIME_B9'),
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding")))

r_svysummary(include = c("BABYCARETIME_B0", 'BABYCARETIME_B1', 'BABYCARETIME_B2',
                         'BABYCARETIME_B3', 'BABYCARETIME_B4', 'BABYCARETIME_B5', 
                         'BABYCARETIME_B6', 'BABYCARETIME_B7', 'BABYCARETIME_B8'),
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding")))

# CAREGIVE 
# RACE INSURANCE PARITY
r_svysummary(by = "PARITY", 
             include = "CAREGIVE")

#CAREGIVE
r_svysummary(include = c("CAREGIVE1C1", 'CAREGIVE1C2', 'CAREGIVE1C3',
                         'CAREGIVE1C4', 'CAREGIVE1C5', 'CAREGIVE1C6', 
                         'CAREGIVE1C7', 'CAREGIVE1C8'),
             data = filter(LTM_dsn, CAREGIVE == "Yes"))

# STUDENT 
# RACE INSURANCE DISABILITY 
r_svysummary(by = "DISABILITY",
             include = "STUDENT_R")

# RACE INSURANCE DISABILITY 
r_svysummary(#by = "RACE",
  include = "PARTSTUDENT_R")

r_svysummary(include = "TIME_SINCE_BIRTH",
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield")))

# MH2WKMHCOND
r_svysummary(by = "WarmRec",
             include = "PREPREG_MHCONDC2",
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield")))
r_svysummary(by = "WarmRec",
             include = "PREPREG_MHCONDC1",
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield")))
#  RACE
r_svysummary(include = c('PREPREG_MHCONDC2', 
                         'PHQ4_PREG_ANX', 
                         'PHQ4_PPANX', 
                         'PHQ4_MH2WK_ANX'), 
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield")))

r_svysummary(include = c('PREPREG_MHCONDC1', 
                         'PHQ4_PREG_DEP', 
                         'PHQ4_PPDEP', 
                         'PHQ4_MH2WK_DEP'), 
             data = filter(LTM_dsn, 
                           Q2FIELD %in% c("Original Fielding", "Refield")))

# Psych distress
# RACE INSURANCE DISABILITY
r_svysummary(by = "DISABILITY",
             include = "PHQ4_MH2WK_PSYCH")

# English proficiency
r_svysummary(by = "ENGPROF", 
             include = "ResLanguage")

# PPTreatment 
# RACE INSURANCE URBANICITY2 MODE1INDEX AGE4
r_svysummary(by = "RACE",
             include = 'PP_SUPP_ONLY',
             data = filter(LTM_dsn, MH2WK_ANXDEP == 1))

# FLOW CHART -----
r_svysummary(by = "TIME_SINCE_BIRTH_QUART",
               include = "PHQ4_MH2WK_DEP", 
             data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

# PREPREG_MHCONDC2 PHQ4_PREG_ANX PHQ4_PPANX PHQ4_MH2WK_ANX 
count_svysummary(include = "PREPREG_MHCONDC2", 
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

count_svysummary(by = "PREPREG_MHCONDC2", 
                 include = "PHQ4_PREG_ANX",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

# Anxiety, worry, or stress    Not selected
count_svysummary(by = "PHQ4_PREG_ANX", 
                 include = "PHQ4_PPANX",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                                 PREPREG_MHCONDC2 == "Not selected"))

count_svysummary(by = "PHQ4_PPANX", 
                 include = "PHQ4_MH2WK_ANX",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                                 PREPREG_MHCONDC2 == "Not selected",
                               PHQ4_PREG_ANX == "Positive screen for anxiety"))


# PREPREG_MHCONDC1 PHQ4_PREG_DEP PHQ4_PPDEP PHQ4_MH2WK_DEP
count_svysummary(include = "PREPREG_MHCONDC1", 
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

count_svysummary(by = "PREPREG_MHCONDC1", 
                 include = "PHQ4_PREG_DEP",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))

# Depression or sadness    Not selected
count_svysummary(by = "PHQ4_PREG_DEP", 
                 include = "PHQ4_PPDEP",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                                 PREPREG_MHCONDC1 == "Depression or sadness"))

count_svysummary(by = "PHQ4_PPDEP", 
                 include = "PHQ4_MH2WK_DEP",
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield") & 
                                 PREPREG_MHCONDC1 == "Depression or sadness",
                               PHQ4_PREG_DEP == "Positive screen for depression"))


# Breastfeeding 
# PLANNEDFEED_ONLY FEED1WEEK_ONLY ANY_BF_3MONTH EXCL_BF_3MONTH ANY_BF_6MONTH EXCL_BF_6MONTH
count_svysummary(include = "PLANNEDFEED_ONLY", 
                 data = filter(LTM_dsn, Q2FIELD %in% c("Original Fielding", "Refield")))





