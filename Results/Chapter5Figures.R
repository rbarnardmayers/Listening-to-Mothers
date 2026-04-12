setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")


# PP Doula support by race, insurance, urbanicity, and political views
fig_compile("DOULAC3", 
            others = c("RACE", "INSURANCE", "URBANICITY2", "POLIT2")) %>% View()


r_svysummary(include = c("DOULA2C1", 'DOULA2C2', "DOULA2C3", "DOULA2C4", 
                         "DOULA2C5", "DOULA2C6", "DOULA2C7"), 
             data = filter(LTM_dsn, DOULAC3 != "Not selected"))

# PP Visit topic

fig_compile_2(c("VISITTOPIC_A1", "VISITTOPIC_A2", "VISITTOPIC_A3", 
                "VISITTOPIC_A4", "VISITTOPIC_A5", "VISITTOPIC_A6",
                "VISITTOPIC_A7"), others = c("INSURANCE")) %>% 
  subset(!Var %in% c("I’m not sure", "I’d prefer not to answer")) %>% 
  View()

# PP Visit numbers
fig_compile("PPVISIT2") %>% View()
r_svysummary(by = "RACE",include = "PPVISIT2")

r_svysummary(include = "PPVISIT_8")

# for MW vs OB prenatal provider for 0 visits, 
r_svysummary(by = "xMODE1", include = "PPVISIT2")
# and MW vs OB birth attendant for 0 visits, 
# for 4+ visits NICU admission vs not
# mode of birth for reporting in text

# Mental Health 
fig_compile("PP_MSUPPORT_ONLY", 
            data = filter(LTM_dsn, PHQ4_PPDEP == "Positive screen for depression" |
                            PHQ4_PPANX == "Positive screen for anxiety")) %>% 
  t() %>% 
  View()

r_svysummary(by = "RACE", 
             include = "PP_MSUPPORT_ONLY",
             data = filter(LTM_dsn, PHQ4_PPDEP == "Positive screen for depression" |
                             PHQ4_PPANX == "Positive screen for anxiety"))

fig_compile("PP_UNMET_NEEDS") %>% View()
r_svysummary(by = "URBANICITY2", 
             include = "PP_UNMET_NEEDS", 
             data = filter(LTM_dsn, PHQ4_PPDEP == "Positive screen for depression" |
                             PHQ4_PPANX == "Positive screen for anxiety"))

r_svysummary(include = "PHQ4_PPANX")

# Breastfeeding postpartm ONLY
r_svysummary(include = "PLANNEDFEEDC1")

r_svysummary(by = "PLANNEDFEED_ONLY", include = "FEED1WEEK_ONLY")
r_svysummary(by = "FEED1WEEK_ONLY", include = "BF_3MONTH", 
             data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Formula"))


# Breastfeeding postpartm ANY
fig_compile(maincol = "FEED1WEEK_ONLY", 
            others = c("RACE", "INSURANCE", "BIRTHATTEND2", 
                       "xMODE2")) %>% View()

r_svysummary(by = "RACE", 
             include = "FEED1WEEKC2")

r_svysummary(by = "PLANNEDFEED_ONLY", 
             include = "FEED1WEEK_ONLY")

r_svysummary(by = "DISABILITY", 
            include = "FEED1WEEK_ONLY")

r_svysummary(include = "PLANNEDFEEDC1")

r_svysummary(include = "WEAN")
r_svysummary(by = "MODE2023",
             include = "WEAN")

# FLOW CHARTS
count_svysummary(include = "PLANNEDFEED_ONLY")
count_svysummary(include = "FEED1WEEK_ONLY")
count_svysummary(include = "EXCL_BF_3MONTH")
count_svysummary(include = "EXCL_BF_6MONTH", 
                 data = filter(LTM_dsn, EXCL_BF_3MONTH == 1))


count_svysummary(by = "PLANNEDFEED_ONLY", 
                 include = "FEED1WEEK_ONLY")

count_svysummary(by = "FEED1WEEK_ONLY", 
                 include = "EXCL_BF_3MONTH", 
                 data = filter(LTM_dsn, PLANNEDFEED_ONLY == "UNKNOWN"))

count_svysummary(by = "FEED1WEEK_ONLY", 
                 include = "EXCL_BF_6MONTH", 
                 data = filter(LTM_dsn, PLANNEDFEED_ONLY == "UNKNOWN" & 
                                 EXCL_BF_3MONTH == 1))

count_svysummary(include = "PLANNEDFEEDC1")
count_svysummary(include = "FEED1WEEKC1")
count_svysummary(include = "ANY_BF_3MONTH")
count_svysummary(include = "ANY_BF_6MONTH")

count_svysummary(by = "PLANNEDFEEDC1", 
                 include = "FEED1WEEKC1")

count_svysummary(by = "FEED1WEEKC1", 
                 include = "ANY_BF_3MONTH", 
             data = filter(LTM_dsn, PLANNEDFEEDC1 == "Not selected"))

count_svysummary(by = "FEED1WEEKC1", 
                 include = "ANY_BF_6MONTH", 
                 data = filter(LTM_dsn, 
                               PLANNEDFEEDC1 == "Breast milk"))



r_svysummary(by = "xMODE2", 
             include = "EXCLUSIVEBF")

r_svysummary(by = "EMPLOYCAT", 
             include = "EXCLUSIVEBF")

r_svysummary(by = "xMODE2", 
             include = "BF_MONTHS")

r_svysummary(by = "EMPLOYCAT", 
             include = "EXCLUSIVEBF")


# Depression trajectory
r_svysummary(include = "PREPREG_MHCONDC1")
count_svysummary(by = "PREPREG_MHCONDC1", 
                 include = "PHQ4_PREG_DEP")

# 375646.7600/3396402
# 400541.2100/3396402
# 350781.4600/3396402
# 2266184.6000/3396402

count_svysummary(by = "PHQ4_PREG_DEP", 
                 include = "PHQ4_PPDEP", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC1 != "Not selected"))
# 265043.9100/3396402
# 108532.0400/3396402
# 85765.6700/3396402
# 312453.4900/3396402

count_svysummary(by = "PHQ4_PREG_DEP", 
                 include = "PHQ4_PPDEP", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC1 == "Not selected"))

# 176540.9000/3396402
# 172546.5100/3396402
# 292414.8500/3396402
# 1969211.1100/3396402

# Anxiety trajectory
r_svysummary(include = "PREPREG_MHCONDC2")
count_svysummary(by = "PREPREG_MHCONDC2", 
                 include = "PHQ4_PREG_ANX")

# 827916.8500/3396402
# 641877.2200/3396402
# 367666.9600/3396402
# 1553155.2100/3396402

count_svysummary(by = "PHQ4_PREG_ANX", 
                 include = "PHQ4_PPANX", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC2 != "Not selected"))
# 571347.3200/3396402
# 246626.4000/3396402
# 182312.9600/3396402
# 455967.9900/3396402

count_svysummary(by = "PHQ4_PREG_ANX", 
                 include = "PHQ4_PPANX", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC2 == "Not selected"))

# 193499.7300/3396402
# 173991.1100/3396402
# 239068.5900/3396402
# 1311761.3600/3396402



# 
# 5.3.	Concordant BF by race, insurance
fig_5.3a <- fig_compile("FEED_CONCORDANT", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
fig_5.3b <- fig_compile("FEED_CONCORDANT", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Formula"))

fig_5.3a <- fig_5.3a %>% subset(FEED_CONCORDANT == 1) %>% mutate(Group = "Breast")
fig_5.3b <- fig_5.3b %>% subset(FEED_CONCORDANT == 1) %>% mutate(Group = "Formula")

fig_5.3 <- rbind(fig_5.3a, fig_5.3b)

# checking stat sig
r_svysummary(by = "RACE", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
r_svysummary(by = "RACE", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Formula"))

r_svysummary(by = "INSURANCE", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
r_svysummary(by = "INSURANCE", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Formula"))

r_svysummary(by = "MODE2023", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
r_svysummary(by = "MODE2023", include = "FEED_CONCORDANT", data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Formula"))


# CS by Doulas 
r_svysummary(by = "DOULAC1", include = "MODE2023")
r_svysummary(by = "DOULAC2", include = "MODE2023")

# 5.4.	Value added situations?? 

# 5.5.	Hospfeed 

r_svysummary(by = "SUM_HOSPFEED", include = "BFGOAL")


# 5.12.	Social needs postpartum by race and insuranc

fig_5.12 <- fig_compile_2(c("SNABUSE", "SNCHILDCARE", "SNDRUGS", "SNINCOME", 
                            "SNLIVE", "SNMEAL", "SNTRANSPORT", "SNUNSAFE", 
                            "SNUTILITIES")) 
 fig_compile("CAT_SNNEEDS") %>% View()
# SUM_SOCIALNEED
r_svysummary(by = "RACE", include = "CAT_SNNEEDS")
r_svysummary(include = c("SNABUSE", "SNCHILDCARE", "SNDRUGS", "SNINCOME", 
                         "SNLIVE", "SNMEAL", "SNTRANSPORT", "SNUNSAFE", 
                         "SNUTILITIES"))
