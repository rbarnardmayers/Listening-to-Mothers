setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")

# Chapter 2 
r_svysummary(by = "INCCAT2", include = "EMPLOYBEN")
r_svysummary(by = "INCCAT2", include = "EMPLOYCHANGE1")

r_svysummary(by = "RACE", 
             include = "MSUPPORT_ONLY",
             data = filter(LTM_dsn, PHQ4_PREG_ANX == "Positive screen for anxiety" | PHQ4_PREG_DEP == "Positive screen for depression"))

fig_compile("MSUPPORT_ONLY", data = filter(LTM_dsn, PHQ4_PREG_ANX == "Positive screen for anxiety" | PHQ4_PREG_DEP == "Positive screen for depression")) %>% 
  View()

# Social needs
print.cont.groups("DISABILITY", "SUM_SOCIALNEED")
r_svysummary(by = "MARRIED", "SUM_SOCIALNEED")
# INCCAT2, DISABILITY, RACE, MARRIED,URBANICITY2

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
fig_3_21 <- fig_compile("phys_cb", others = c("RACE", "INSURANCE","BIRTHATTEND2" ,'DOULAC2', 'DOULA', 'MIDWIFE_DOULA'))

# Feeding
fig_2.27 <- fig_compile("PLANNEDFEED_ONLY", others = c("RACE", "INSURANCE", "URBANICITY2"))
r_svysummary(by = "RACE", include = "PLANNEDFEED_ONLY")

# fig_3.23 <- collapse.2by2(c("HOSPFEEDC1", "HOSPFEEDC2", "HOSPFEEDC3", "HOSPFEEDC4",
#                             "HOSPFEEDC5", "HOSPFEEDC6", "HOSPFEEDC7", "HOSPFEEDC9",
#                             "HOSPFEEDC10"))
fig_3.23 <- fig_compile_2(c("HOSPFEEDC1", "HOSPFEEDC2", "HOSPFEEDC3", "HOSPFEEDC4",
                            "HOSPFEEDC5", "HOSPFEEDC6", "HOSPFEEDC7", "HOSPFEEDC9",
                            "HOSPFEEDC10", "HOSPFEEDC11"), others = "FEED1WEEK_ONLY") %>% 
  subset(Var != "Not selected")


#They were more likely to achieve their intention if they were helped to get started 
# breastfeeding as soon after birth as they and their newborn were ready, if their 
# babies were not given formula or water supplements in the hospital, and if 
# they were not given formula samples, coupons, or offers
r_svysummary(by = "HOSPFEEDC4", 
             include = "FEED1WEEK_ONLY",
             data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
# Among those who intended to exclusively BF...
# 83% who experienced help exclusively fed at 1 week 
# 72% who did not experience help exclusively fed at 1 week (p < 0.05)

# 89% of those who did not have their baby get formula to supplement exclusively fed at 1 week
# 58% of those whose baby did get formula or water (p < 0.05)

# 88% of those who did not get free formula samples exclusively breastfed 
# 70% of those who did get free samples (p < 0.05)

r_svysummary(by = "PLANNEDFEED_ONLY",
             include = c("HOSPFEEDC1", "HOSPFEEDC2", "HOSPFEEDC3", "HOSPFEEDC4",
                         "HOSPFEEDC5", "HOSPFEEDC6", "HOSPFEEDC7", "HOSPFEEDC8",
                         'HOSPFEEDC9', "HOSPFEEDC10", "RHOSPFEEDC11"))


r_svysummary(by = "PLANNEDFEED_ONLY", 
             include = c("HOSPFEEDC1", "HOSPFEEDC2", "HOSPFEEDC3", "HOSPFEEDC4",
                         "HOSPFEEDC5", "HOSPFEEDC6", "HOSPFEEDC7", "HOSPFEEDC8",
                         'HOSPFEEDC9', "HOSPFEEDC10", "RHOSPFEEDC11"), 
             data = filter(LTM_dsn, PARITY == "Nulliparous"))

r_svysummary(by = "PLANNEDFEED_ONLY", 
             include = "HOSPFEEDC8", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth"))
# CS: 6 (0.0297, 0.1220)
# V: 2 (0.0129, 0.0295)


# feeding exclus. BF at 1 week by each hosp feed cat
LTM_dsn %>% 
  filter(PLANNEDFEED_ONLY == "Breastmilk") %>%
  tbl_svysummary(by = "FEED1WEEK_ONLY",
                 include = c("HOSPFEEDC1", "HOSPFEEDC2", "HOSPFEEDC3", "HOSPFEEDC4",
                             "HOSPFEEDC5", "HOSPFEEDC6", "HOSPFEEDC7", "HOSPFEEDC8",
                             'HOSPFEEDC9', "HOSPFEEDC10", "RHOSPFEEDC11"), 
                 percent = "row")

r_svysummary(by = "HOSPFEED2", 
             include = "FEED1WEEK_ONLY", 
             data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))
# 
r_svysummary(by = "RACE",
             include = "OTHERSUPPORT_ONLY")

# Babies of those who planned mixed feeding more frequently (48%) were given
# formula or water supplements than those who planned exclusive breastfeeding 
# (25%) (n.s. or p < 0.05?). Respondents who planned mixed feeding (54%) were 
# more frequently given free formula samples, coupons, or offers than those who 
# planned exclusive breastfeeding (38%) (n.s. or p < 0.05?).

fig.3.23b <- fig_compile("SUM_HOSPFEED", others = "FEED1WEEK_ONLY")
LTM_dsn %>% tbl_svysummary(by = "PLANNEDFEED_ONLY", include = "FEED1WEEK_ONLY")

r_svysummary(include = "LEARNED2_R")

# NICU and gestage
r_svysummary(by = "ANYNICU", include = "xGESTAGE_R")

# planned birth 
LTM_dsn %>% tbl_svysummary(include = "PLANNED_INDUCED")

##### FLOWCHART ####
# No prior CS, term (37 – 41) birth
fc <- LTM_dsn %>% 
  filter(MODE_ALL == 0 & GESTAGE_R >= 37 & GESTAGE_R <= 41)
r_svysummary(by = "BIGBABY1", include = c("MEDINDUCE", "MODE2023"), data = fc)

r_svysummary(by = "MEDINDUCE", include = "MODE2023", 
             data = fc)

# 3.10	SDM in induction: to what extent are various subgroups experiencing SDM, by race and ethnicity, private vs Medicaid insurance
fig_3_10 <- fig_compile_2(c("INDUCE1", "INDUCE2", "INDUCE3", "INDUCE4"), others = "RACE")
fig_3_10 <- fig_compile("DEC_MAKE", others = c("RACE", "BIRTHATTEND2", "PARITY", "BIRTHCOUNTRY", "DISABILITY"))
# Let's show RE, along with birth attendant, parity, nativity, disability status

# Painmeds ----
fig_3pain <- fig_compile_2(c("PAINMEDSC1", "PAINMEDSC2", "PAINMEDSC3", 
                             "PAINMEDSC4", "PAINMEDSC5", "PAINMEDSC6", "PAINMEDSC7"), 
                           others = "INSURANCE", 
                           data = filter(LTM_dsn, MODE2023 == "Vaginal birth")) %>% 
  select(c("Var", "prop")) %>% 
  subset(Var != "Not selected")

fig_3painany <- fig_compile("PAINMEDSANY", 
                            others = c("RACE", "PARITY", "BIRTHATTEND2", "DOULA", "DOULAC2"), 
                            data = filter(LTM_dsn, 
                                          MODE2023 == "Vaginal birth"))
r_svysummary(by = 'DOULAC2', include = 'PAINMEDSANY', 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth"))

r_svysummary(include = "AROM_ANY")
r_svysummary(by = "AROM_ANY", include = "MODE2023")
r_svysummary(include = "BLADDER")

r_svysummary(by = "CSECTIONTYPE", 
             include = c("PAINMEDSC1", "PAINMEDSC2", "PAINMEDSC3", 
                         "PAINMEDSC4", "PAINMEDSC5", "PAINMEDSC6", "PAINMEDSC7"),
             data = filter(LTM_dsn, MODE2023 == "Cesarean birth (c-section)"))
  
  # Labor int ----
fig3_laborint <- print.fig("LABORINT_ALL")
fig3_laborintnone <- print.fig("R_LABORINTC6")
fig3_laborintall <- collapse.fun(c('LABORINTC1', 'LABORINTC2', 'LABORINTC3', 
                                   'LABORINTC4', 'LABORINTC5'))

fig3_augment <- fig_compile("AROM",data = filter(LTM_dsn, MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))
r_svysummary(include = "PITOCIN")

# Position by doula 
fig_3.position <- fig_compile("POSITION", others = c("DOULAC2"))

# Intake restrictions 
r_svysummary(include = "LABORPERMIT_A1")
tabl_labperm <- fig_compile_2(c("LABORPERMIT_A1", "LABORPERMIT_A2"), 
                              others = "INSURANCE")

r_svysummary(by = "LABORPERMIT_A1", include = "LABORINTC2" )
r_svysummary(by = "LABORPERMIT_A2", include = "LABORINTC2" )

fig3.drink <- fig_compile("LABORINTC2", "LABORPERMIT_A1")
fig3.eat <- fig_compile("LABORINTC2", "LABORPERMIT_A2")

# pcmc scores 
r_svysummary(by = "RACE", 
             include = "PCMC_SCORE_R")
r_svysummary(include = c("PCMC_comms", 'PCMC_resp', "PCMC_supp"))
r_svysummary(by = "DISABILITY", include = c("CUSTOMS"))
r_svysummary(by = 'RACE', include = c("CUSTOMS_subopt"))

r_svysummary(include = c('RESPECT', 'KNOWLEDGE', 'HEARD',
                         'DECISIONS', 'CONSENT', 'INFORMED',
                         'TRUST', 'FEEDING', 'SAFE', 'TIMELINESS',
                         'DISCRIMINATION_pcmc', 'NEGLECT_pcmc'))

# Doula rates table 3.1

r_svysummary(by = "DOULA", 
             include = c("DOULAC1", "DOULAC2", "DOULAC3"))

# CHAPTER 4 


# Cesarean rates for Cervical dilation cats
r_svysummary(by = "VAGEXAM_5", include = "MODE2023")
r_svysummary(by = "VAGEXAM2", include = "MODE2023")

# EFM and Walking and Epidural
r_svysummary(by = "FETALMONC1", include = "LABORWALK")
r_svysummary(by = "PAINMEDSC1", include = "LABORWALK")

# Customs by race
fig_compile("CUSTOMS_subopt", 
            others = c("RACE", "INSURANCE", "DISABILITY")) %>% View()

fig_compile("CUSTOMS2", 
            others = c("RACE", "INSURANCE", "DISABILITY"), 
            ) %>% View()

fig_compile("CUSTOMS2", 
            others = c("DISABILITY")) %>% View()

r_svysummary(by = "DISABILITY", 
             include = c("CUSTOMS2", "CUSTOMS_subopt"))


# Looking at fetal monitoring
r_svysummary(include = "FETALMON_ONLY")

# bladder catehtors by epideural or not
r_svysummary(by = "PAINMEDSC1", include = "LABORINTC4", 
             data = filter(LTM_dsn, 
                           MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))

# Those whose providers recommended against induction (32%) were less
# likely to have an induction than those whose providers made no recommendation 
# (46%), 

r_svysummary(by = "INDUCE5", include = "MEDINDUCE", 
             data = filter(LTM_dsn, INDUCE == "Yes"))

# less likely to have this elective procedure than those whose providers 
# recommended having it (67%) (58.7%, 73.5%)

# medinduce 2 
r_svysummary(include = c("MEDINDUCE1C1","MEDINDUCE1C2","MEDINDUCE1C3",
                         "MEDINDUCE1C4","MEDINDUCE1C5","MEDINDUCE1C6",
                         "MEDINDUCE1C7"),
             data = filter(LTM_dsn, 
                           MODE2023 == "Vaginal birth" & MEDINDUCE2 == "No"))

r_svysummary(include = "CSECTIONTYPE",
             data = filter(LTM_dsn, 
                           MODE2023 == "Vaginal birth" & MEDINDUCE2 == "No"))



