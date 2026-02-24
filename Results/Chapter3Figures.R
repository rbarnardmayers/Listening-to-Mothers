setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")

# 3.1	GA vertical bars by gestational week or other viz to show how much we have deviated from a bell curve
LTM_dsn %>%
  tbl_svysummary(include = GESTAGE_R,
                 statistic = list(all_categorical() ~ "{n}"))

# 3.2	 GOLDENHOUR segmented bar graph for percentage with mom, spouse/partner, staff/routine, staff/special, I’m not sure,
fig_3_2 <- fig_compile("GOLDENHOUR", others = c("RACE", "INSURANCE", "MODE2023", "BIRTHATTEND", "LANGSIMP"))

# 3.3	SKIN: simple bar graph with percentage with mom/spouse/partner and skin-to-skin, 
fig_3_3 <- fig_compile("SKIN", others = c("RACE", "MODE2023", "BIRTHATTEND", "LANGSIMP")) 

# 3.4	 NICU: entire time, part time (segmented or stacked bars? Percentage of babies in NICU part-time or throughout with low-risk characteristics (Could be segmented or stacked (pairs) bars) 
fig_3_4 <- fig_compile("NICU", others = c("xBABYHOSP", "BW_CAT", "xGESTAGE"))
# maybe no maternal complications,

# 3.5	 BIRTHATTEND: best to be parallel to 2.3 (prenatal provider: has doctor, midwife, other
fig_3_5 <- fig_compile("BIRTHATTEND2")

# Physiologic childbirth
fig_3_21 <- fig_compile("phys_cb")

# NICU and gestage
r_svysummary(by = "ANYNICU", include = "xGESTAGE_R")


##### FLOWCHART ####
# No prior CS, term (37 – 41) birth
fc <- LTM_dsn %>% 
  filter(MODE_ALL == 0 & GESTAGE_R >= 37 & GESTAGE_R <= 41)
r_svysummary(by = "BIGBABY1", include = c("MEDINDUCE", "MODE2023"), data = fc)


# 3.10	SDM in induction: to what extent are various subgroups experiencing SDM, by race and ethnicity, private vs Medicaid insurance
fig_3_10 <- fig_compile_2(c("INDUCE1", "INDUCE2", "INDUCE3", "INDUCE4"), others = "RACE")
fig_3_10 <- fig_compile("DEC_MAKE", others = c("RACE", "BIRTHATTEND2", "PARITY", "BIRTHCOUNTRY", "DISABILITY"))
# Let's show RE, along with birth attendant, parity, nativity, disability status

# Customs by race
r_svysummary(by = "RACE", include = "CUSTOMS")
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



# 5.4.	Value added situations?? 

# 5.5.	Hospfeed 

r_svysummary(by = "SUM_HOSPFEED", include = "BFGOAL")

# 5.11.	Received help by screened for PHQ4 by race and insurance
fig_5.11a <- fig_compile("PP_MSUPPORT_ANY", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PHQ4_PPPSYCH %in% c("Mild (3-5)", "Moderate (6-8)", "Severe (9-12)")))
fig_5.11b <- fig_compile("PP_MSUPPORT_ANY", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PHQ4_PPPSYCH == "Normal (0-2)"))
fig_5.11c <- fig_compile("PP_MSUPPORT_ANY", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PHQ4_PPPSYCH == "Mild (3-5)"))
fig_5.11d <- fig_compile("PP_MSUPPORT_ANY", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PHQ4_PPPSYCH == "Moderate (6-8)"))
fig_5.11e <- fig_compile("PP_MSUPPORT_ANY", others = c("RACE", "INSURANCE", "MODE2023"), data = filter(LTM_dsn, PHQ4_PPPSYCH == "Severe (9-12)"))

fig_5.11a <- fig_5.11a %>% subset(PP_MSUPPORT_ANY == 1) %>% mutate(Group = "Any")
fig_5.11b <- fig_5.11b %>% subset(PP_MSUPPORT_ANY == 1) %>% mutate(Group = "Normal")
fig_5.11c <- fig_5.11c %>% subset(PP_MSUPPORT_ANY == 1) %>% mutate(Group = "Mild")
fig_5.11d <- fig_5.11d %>% subset(PP_MSUPPORT_ANY == 1) %>% mutate(Group = "Moderate")
fig_5.11e <- fig_5.11e %>% subset(PP_MSUPPORT_ANY == 1) %>% mutate(Group = "Severe")

fig_5.11 <- rbind(fig_5.11a, fig_5.11b, fig_5.11c, fig_5.11d, fig_5.11e)

r_svysummary(by = "RACE", include ="PP_MSUPPORT_ANY", data = filter(LTM_dsn, PHQ4_PPPSYCH %in% c("Mild (3-5)", "Moderate (6-8)", "Severe (9-12)")))


# 5.12.	Social needs postpartum by race and insurance
fig_5.12 <- fig_compile_2(c("SNABUSE", "SNCHILDCARE", "SNDRUGS", "SNINCOME", 
                            "SNLIVE", "SNMEAL", "SNTRANSPORT", "SNUNSAFE", 
                            "SNUTILITIES")) 
fig_5.12 <- fig_compile("SUM_SNNEEDS")

r_svysummary(by = "RACE", include = "CAT_SNNEEDS")

# 5.13.	Continuing social needs byt counts, race and insurance


