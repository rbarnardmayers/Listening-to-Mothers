setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")

# Figure 5.1
# RACE, INSURANCE, URBANICITY2, POLIT2
r_svysummary(by = "RACE",
             include = "DOULAC3")

# Figure 5.2
r_svysummary(include = c("DOULA2C1", "DOULA2C2", "DOULA2C3", 
                         "DOULA2C4", "DOULA2C5"))

# Figure 5.3
# RACE, INSURANCE
r_svysummary(by = "RACE",
             include = "PPVISIT2")

# Figure 5.4
fig_compile_2(c("VISITTOPIC_A1", "VISITTOPIC_A2", "VISITTOPIC_A3", 
                "VISITTOPIC_A4", "VISITTOPIC_A5", "VISITTOPIC_A6",
                "VISITTOPIC_A7"), others = c("INSURANCE")) %>% 
  subset(!Var %in% c("I’m not sure", "I’d prefer not to answer")) %>% 
  View()

# Figure 5.5
# RACE, INSURANCE, BIRTHATTEND, xMODE2
r_svysummary(by = "RACE", 
             include = "FEED1WEEK_ONLY")

# Figure 5.6
# FLOW CHARTS
count_svysummary(include = "PLANNEDFEED_ONLY")
count_svysummary(include = "FEED1WEEK_ONLY")
count_svysummary(include = "EXCL_BF_3MONTH", 
                 data = filter(LTM_dsn, FEED1WEEK_ONLY == "Breastmilk" & MONTH_3 == 1))
count_svysummary(include = "EXCL_BF_6MONTH", 
                 data = filter(LTM_dsn, EXCL_BF_3MONTH == 1 & MONTH_6 == 1))

count_svysummary(by = "PLANNEDFEED_ONLY", 
                 include = "FEED1WEEK_ONLY")

count_svysummary(by = "FEED1WEEK_ONLY", 
                 include = "EXCL_BF_3MONTH", 
                 data = filter(LTM_dsn, PLANNEDFEED_ONLY == "UNKNOWN" & MONTH_3 == 1))

count_svysummary(by = "FEED1WEEK_ONLY", 
                 include = "EXCL_BF_6MONTH", 
                 data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk" & 
                                 EXCL_BF_3MONTH == 1 & MONTH_6 == 1))

# Figure 5.7
count_svysummary(include = "PLANNEDFEEDC1")
count_svysummary(include = "FEED1WEEKC1")
count_svysummary(include = "ANY_BF_3MONTH", 
                 data = filter(LTM_dsn, MONTH_3 == 1))
count_svysummary(include = "ANY_BF_6MONTH", 
                 data = filter(LTM_dsn, MONTH_6 == 1))

count_svysummary(by = "PLANNEDFEEDC1", 
                 include = "FEED1WEEKC1")

count_svysummary(by = "FEED1WEEKC1", 
                 include = "ANY_BF_3MONTH", 
                 data = filter(LTM_dsn, PLANNEDFEEDC1 == "Breast milk" & 
                                 MONTH_3 == 1))

count_svysummary(by = "FEED1WEEKC1", 
                 include = "ANY_BF_6MONTH", 
                 data = filter(LTM_dsn, 
                               PLANNEDFEEDC1 == "Breast milk" & 
                                 MONTH_6 == 1 & ANY_BF_3MONTH == 1))

# Figure 5.8
# RACE, INSURANCE, xMODE2
r_svysummary(#by = "INSURANCE", 
  include = "EXCLUSIVEBF",
  data = filter(LTM_dsn, FEED1WEEK_ONLY == "Breastmilk" &
                  (CURRENTFEEDC2 == "Formula" | 
                     CURRENTFEEDC3 == "Other liquids (e.g., cow’s milk, juice)" | 
                     CURRENTFEEDC4 == "Solid food")))

# Figure 5.9
# RACE, INSURANCE, MODE2023
r_svysummary(by = "INSURANCE", 
             include = "WEAN")

# Figure 5.10
# RACE, INSURANCE, PARITY, MODE2023, URBANICITY2
r_svysummary(by = "RACE", 
             include = "EXCLBFGOAL")

# Figure 5.11
# RACE, INSURANCE, PARITY, MODE2023
r_svysummary(by = "RACE", 
             include = "BFGOAL")

# Figure 5.12
# Anxiety trajectory
count_svysummary(include = "PREPREG_MHCONDC2")
count_svysummary(by = "PREPREG_MHCONDC2", 
                 include = "PHQ4_PREG_ANX")

# 827916.8500/3396402
# 641877.2200/3396402
# 367666.9600/3396402
# 1553155.2100/3396402

count_svysummary(by = "PHQ4_PREG_ANX", 
                 include = "PHQ4_PPANX", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC2== "Not selected"))
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


# Figure 5.13
# Depression trajectory
count_svysummary(include = "PREPREG_MHCONDC1")
count_svysummary(by = "PREPREG_MHCONDC1", 
                 include = "PHQ4_PREG_DEP")

# 375646.7600/3396402
# 400541.2100/3396402
# 350781.4600/3396402
# 2266184.6000/3396402

count_svysummary(by = "PHQ4_PREG_DEP", 
                 include = "PHQ4_PPDEP", 
                 data = filter(LTM_dsn, 
                               PREPREG_MHCONDC1 == "Not selected"))
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


# Figure 5.14
# RACE, INSURANCE, DISABILITY
r_svysummary(by = "RACE", 
             include = "PHQ4_PPANX")

# Figure 5.15
# RACE, INSURANCE, DISABILITY
r_svysummary(by = "RACE", 
             include = "PHQ4_PPDEP")

# Figure 5.16
# RACE, INSURANCE, MODE2023, PARITY, URBANICITY2
r_svysummary(by = "RACE", 
             include = "PHQ4_PPPSYCH")

# Figure 5.17
# RACE, INSURANCE, URBANICITY2
r_svysummary(by = "RACE", 
             include = "PP_MSUPPORT_ONLY")

# Figure 5.18
# RACE, INSURANCE, URBANICITY
r_svysummary(by = "RACE", 
             include = "PP_UNMET_NEEDS")

# Figure 5.19
count_svysummary(include = c("SNABUSE", "SNCHILDCARE", "SNDRUGS", "SNINCOME", 
                             "SNLIVE", "SNMEAL", "SNTRANSPORT", "SNUNSAFE", 
                             "SNUTILITIES"))

# Figure 5.20
# RACE, INSURANCE, URBANICITY2
r_svysummary(include = "CAT_SNNEEDS")



# Checking number of cesareans ----
LTM_final %>% subset(xMODE2 == "Cesarean Repeat") %>% 
  select(c(NUM_CS, NUMB_BIRTH,MODE_ALL, RMODE, RMODE2,RMODE3, RMODE4, 
           RMODE5, RMODE6, RMODE7, RMODE8, RMODE9, RMODE10, RMODE11)) %>% 
  View() 


