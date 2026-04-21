source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")

# Figure 3.1	
LTM_dsn %>%
  tbl_svysummary(include = GESTAGE_R,
                 statistic = list(all_categorical() ~ "{n}"))

# Figure 3.2	
fig_compile("GOLDENHOUR", 
                       others = c("RACE", "INSURANCE", "MODE2023", 
                                  "BIRTHATTEND", "LANGSIMP")) %>% 
  View()

# Figure 3.3	
fig_compile("SKIN",
            others = c("RACE", "MODE2023", "BIRTHATTEND", "LANGSIMP")) %>% 
  View()

# Figure 3.4	
fig_compile("ANYNICU", 
            others = c("xBABYHOSP", "BW_CAT", "xGESTAGE")) %>% 
  View()

# Figure 3.5 
r_svysummary(by = "ANYNICU", 
             include = c("xGESTAGE_R", "BW_CAT", "xBABYHOSP"))


# Figure 3.6
# Overall and MODE2023
r_svysummary(by = "MODE2023", 
             include = "BIRTHATTEND")


# Figure 3.7
# RACE, INSURANCE, PROVIDER2, MODE2023
r_svysummary(by = "INSURANCE", 
             include = "BIRTHATTEND")

# Figure 3.8 
# DOULA VENN DIAGRAM
r_svysummary(include = "DOULA_CAROL")
r_svysummary(include = "DOULA_CAROL", 
             data = filter(LTM_dsn, DOULA == "Yes"))


# Figure 3.9
# RACE, INSURANCE, URBANICITY2, PROVIDER2
r_svysummary(by = "RACE", 
             include = "DOULAC2")

# Figure 3.10
r_svysummary(by = "RACE",
             include = "OTHERSUPPORT_ONLY")

# Figure 3.11
r_svysummary(include = c("INDUCE1", "INDUCE2", "INDUCE3", "INDUCE4"), 
             data = filter(LTM_dsn, INDUCE == "Yes"))


# Figure 3.12
fig_compile("DEC_MAKE", others = c("RACE", "BIRTHATTEND2", "PARITY", 
                                   "BIRTHCOUNTRY", "DISABILITY")) %>% 
  View()

# Figure 3.13
# RACE, INSURANCE, PROVIDER
r_svysummary(by = "RACE", 
             include = "INDUCE5")

# Figure 3.14
r_svysummary(by = "INDUCE5", 
             include = "MEDINDUCE")

# Figure 3.15
r_svysummary(include = c("SELFINDUCE2C1", "SELFINDUCE2C2","SELFINDUCE2C3",
             "SELFINDUCE2C4", "SELFINDUCE2C5", "SELFINDUCE2C6", 
             "SELFINDUCE2C7", "SELFINDUCE2C8"), 
             data = filter(LTM_dsn, SELFINDUCE == "Yes"))

# Figure 3.16
r_svysummary(include = c("MEDINDUCE1C1", "MEDINDUCE1C2", "MEDINDUCE1C3", 
                         "MEDINDUCE1C4", "MEDINDUCE1C5", "MEDINDUCE1C6", 
                         "MEDINDUCE1C7", "MEDINDUCE1C8"), 
             data = filter(LTM_dsn, MEDINDUCE == "Yes"))

# Figure 3.17
r_svysummary(include = c("xMEDINDUCE4", "xMEDINDUCE5"),
             data = filter(LTM_dsn, MEDINDUCE == "Yes"))

# Figure 3.18
r_svysummary(include = c("MEDINDUCE3C1", "MEDINDUCE3C2", "MEDINDUCE3C3", 
                         "MEDINDUCE3C4", "MEDINDUCE3C5", "MEDINDUCE3C6", 
                         "MEDINDUCE3C7", "MEDINDUCE3C8"), 
             data = filter(LTM_dsn, MEDINDUCE == "Yes"))

# Figure 3.19
r_svysummary(include = c('DRUGFREEC1', 'DRUGFREEC2', 'DRUGFREEC3', 'DRUGFREEC4',
                         'DRUGFREEC5', 'DRUGFREEC6', 'DRUGFREEC7', 'DRUGFREEC8',
                         'DRUGFREEC9', 'DRUGFREEC10'), 
             data = filter(LTM_dsn, LABORED == 1))

# Figure 3.20
#IDK HOW THIS WAS CALCULATED

# Figure 3.21
# RACE, PARITY, BIRTHATTEND2, DOULAC2, 
r_svysummary(by = "RACE", 
             include = "DRUGFREEC11", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth"))

# Figure 3.22
r_svysummary(include = c("PAINMEDSC1", "PAINMEDSC2", "PAINMEDSC3",
                         "PAINMEDSC4", "PAINMEDSC5", "PAINMEDSC6", 
                         "PAINMEDSC7"))

# Figure 3.23
# MODE2023, 
r_svysummary(by = "MODE2023", 
             include = c("PAINMEDSC1", "PAINMEDSC2", "PAINMEDSC3",
                         "PAINMEDSC4", "PAINMEDSC5", "PAINMEDSC6", 
                         "PAINMEDSC7"))

r_svysummary(by = "CSECTIONTYPE", 
             include = c("PAINMEDSC1", "PAINMEDSC2", "PAINMEDSC3",
                         "PAINMEDSC4", "PAINMEDSC5", "PAINMEDSC6", 
                         "PAINMEDSC7"), 
             data = filter(LTM_dsn, MODE2023 == "Cesarean birth (c-section)"))

# Figure 3.24
r_svysummary(by = "VAGEXAM2", 
             include = "MODE2023", 
             data =filter(LTM_dsn, LABORED == 1 & VAGEXAM < 90))

# Figure 3.25
count_svysummary(include = c("LABORPERMIT_A1", "LABORPERMIT_A2"), 
                 data = filter(LTM_dsn, LABORED == 1))

# Figure 3.26
r_svysummary(include = c("VAGEXAM_5"))
r_svysummary(include = c("FETALMONC1", "LABORINTC5",
                         "LABORINTC2", "LABORWALK", "LABORINTC4",
                         "LABORINTC3", "DENIED", "LABORINTC1"), 
             data = filter(LTM_dsn, LABORED == 1))

# Figure 3.27
r_svysummary(include = "POSITION", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth"))

# Figure 3.28
r_svysummary(include = "EPISTCHOICE", 
             data = filter(LTM_dsn, EPIST == "Yes"))

# Figure 3.29
r_svysummary(include = "CUM_INT")

# Figure 3.30
fig_compile("phys_cb", others = c("RACE", "INSURANCE","BIRTHATTEND2" ,
                                  'DOULAC2', 'DOULA', 'MIDWIFE_DOULA')) %>% 
  View()

# Figure 3.31
count_svysummary(by = "PLANNEDFEED_ONLY", 
                 include = "FEED1WEEK_ONLY")

# Figure 3.32
r_svysummary(by = "SUM_HOSPFEED", 
             include = "FEED1WEEK_ONLY", 
             data = filter(LTM_dsn, PLANNEDFEED_ONLY == "Breastmilk"))

# Figure 3.33
r_svysummary(include = c('RESPECT', 'KNOWLEDGE', 'HEARD',
                         'DECISIONS', 'CONSENT', 'INFORMED',
                         'TRUST', 'FEEDING', 'SAFE', 'TIMELINESS',
                         'DISCRIMINATION_pcmc', 'NEGLECT_pcmc'))

# Figure 3.34
r_svysummary(include = c(paste0("DISCRIMINATION1C", rep(1:17))), 
             data = filter(LTM_dsn, DISCRIMINATION %in% c("Yes, a few times", 
                                                          "Yes, most of the time",
                                                          "Yes, all the time")))

# Figure 3.35

r_svysummary(by = "DISABILITY", 
             include = c("CUSTOMS"), 
             data = filter(LTM_dsn, CUSTOMS %in% c("No, never", "Yes, a few times", 
                                                   "Yes, all the time", "Yes, most of the time")))

r_svysummary(by = 'DISABILITY', 
             include = c("CUSTOMS_subopt"), 
             data = filter(LTM_dsn, CUSTOMS %in% c("No, never", "Yes, a few times", 
                                                   "Yes, all the time", "Yes, most of the time")))


