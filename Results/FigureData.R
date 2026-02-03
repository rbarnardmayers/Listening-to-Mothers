setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

# Data manip for figures ----
# PPVISIT categorized as 0, 1, 2, 3, 4+ 

# CHAPTER 1 ----
# Provider Type ----
# Percent of racial distribution for each provider
fig1 <- fig_compile("PROVIDER2", c("RACE", "INSURANCE"))

# Provider Choice ----
fig2 <- fig_compile("PROVIDERCHOICE", c("RACE", "INSURANCE"))

# Doula during pregnancy ----
# DOULAC1	== 1 
fig3 <- fig_compile_2(c("DOULA", "DOULAC1", "DOULAC2", "DOULAC3"), c("RACE", "INSURANCE"))

LTM_dsn %>% tbl_svysummary(include = c('DOULA1C1','DOULA1C2', 'DOULA1C3',
                                       'DOULA1C4', 'DOULA1C5', 'DOULA1C6'), 
                           statistic = list(all_categorical() ~ "{p}%")) 

# First visit ----
# FIRSTVISIT 1 == YES, 2 == NO
fig4 <- fig_compile('FIRSTVISIT', c("RACE", "INSURANCE"))

# Reason for no prenatal ----
fig5 <- fig_compile_2(c('NOPRENATALC1','NOPRENATALC2','NOPRENATALC3','NOPRENATALC4',
                        'NOPRENATALC5','NOPRENATALC6','NOPRENATALC7','NOPRENATALC8',
                        'NOPRENATALC9','NOPRENATALC10','NOPRENATALC11','NOPRENATALC12',
                        'NOPRENATALC13')) %>% 
  subset(Var != "Not selected")

# Care setting ----
fig6 <- fig_compile_2(c('CARESETTINGC1','CARESETTINGC2','CARESETTINGC3','CARESETTINGC4',
                        'CARESETTINGC5','CARESETTINGC6','CARESETTINGC7','CARESETTINGC8')) %>% 
  subset(Var != "Not selected")

# CARETYPE1 ----
# CARETYPE1	Did you have a choice about whether to have office prenatal visits by yourself or group prenatal visits?
fig7 <- fig_compile('CARETYPE1', c("RACE", "INSURANCE"))

# CARETYPEPREF ----
# CARETYPEPREF	Which type of prenatal care do you prefer?
# 1	Prenatal visits by myself
# 2	Group prenatal visits
# 3	I like a mix of both
# 99	I’d prefer not to answer

fig8 <- fig_compile('CARETYPEPREF')

# WHYTELE ----
# WHYTELEC1	  1 = Transportation to in-person care was a challenge
# WHYTELEC2	  2 = I wanted to save travel time
# WHYTELEC3	  3 = I had childcare responsibilities
# WHYTELEC4	  4 = I wanted to avoid exposure to COVID-19
# WHYTELEC5	  5 = I was more comfortable with remote than office visits
# WHYTELEC6	  6 = My maternity care provider preferred televisits
# WHYTELEC7	  95 = Other, please specify
fig9a <- fig_compile_2(c('CAREMODEC1', 'CAREMODEC2')) %>% as.data.frame() %>% 
  subset(Var != "Not selected")
fig9 <- fig_compile_2(c('WHYTELEC1','WHYTELEC2','WHYTELEC2','WHYTELEC4',
                        'WHYTELEC5','WHYTELEC6','WHYTELEC7'))

# # BPCONFID+URINECONFID+WEIGHCONFID+BABYHRCONFID ----
# # 1	Yes, fully confident
# # 2	Yes, somewhat confident
# # 3	No, not confident
# # 99	I’d prefer not to answer
# 
# test <- data.frame()
# for(i in c("BPCONFID", "URINECONFID", "WEIGHCONFID",
#            "BABYHRCONFID")){
#   test <- print.fig(i) %>% 
#     mutate(VAR = i) %>%
#     rename("value" = i) 
#   fig10 <- rbind(fig10, test)
# }


# CAREMODEPREF ----
fig10 <- fig_compile('CAREMODEPREF')

# EDUIMPACT ----
fig11 <- fig_compile_2(c('EDUCIMPACTC1','EDUCIMPACTC2','EDUCIMPACTC3',
                         'EDUCIMPACTC4','EDUCIMPACTC5','EDUCIMPACTC6',
                         'EDUCIMPACTC7'))

# PREGCONDITION ----
fig12 <-fig_compile_2(c('PREGCONDITIONC1','PREGCONDITIONC2','PREGCONDITIONC3',
                        'PREGCONDITIONC4','PREGCONDITIONC5','PREGCONDITIONC6',
                        'PREGCONDITIONC7','PREGCONDITIONC8','PREGCONDITIONC9',
                        'PREGCONDITIONC10'))

# Depression ----
# PREPREG_MHCONDC1
fig13 <- fig_compile('PREPREG_MHCONDC1')

# Anxiety ----
# PREPREG_MHCONDC2
fig14 <- fig_compile('PREPREG_MHCONDC2')

# MENTALSUPPORT ----
# MENTALSUPPORT

fig15 <- fig_compile('MSUPPORT_ANY')

# Social needs ----
fig16 <- fig_compile_2(c('SOCIALNEEDC1','SOCIALNEEDC2','SOCIALNEEDC3',
                         'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                         'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                         'SOCIALNEEDC10','SOCIALNEEDC11'))
fig17b <- print.cont.groups("RACE", "SUM_SOCIALNEED") %>% 
  as.data.frame() %>% select(c(Groups, Mean))
fig17c <- print.cont.groups("INCCAT", "SUM_SOCIALNEED") %>% 
  as.data.frame() %>% select(c(Groups, Mean))
fig17a <- print.cont.from.bases("SUM_SOCIALNEED")%>% mutate(Groups = "Total") %>% select(c(Groups, Mean)) 
rownames(fig17a) <- NULL
rownames(fig17b) <- NULL
rownames(fig17c) <- NULL
fig17 <- rbind(fig17a, fig17b, fig17c)

# EMPLOYBEN ----
fig18 <- fig_compile('EMPLOYBEN')

# BIGBABY1 == 1 ----
# BIGBABY1
fig19a <- fig.2by2("BIGBABY1", "MODE2023", 
                   data = subset(LTM_dsn, BIGBABY1 %in% c("Yes", "No"))) %>% as.data.frame()
fig19b <- fig.2by2("BIGBABY1", "INDUCE", 
                   data = subset(LTM_dsn, BIGBABY1 %in% c("Yes", "No") & 
                                   INDUCE %in% c("Yes", "No"))) %>% as.data.frame()
fig19 <- cbind(fig19a, fig19b)

fig20 <- fig_compile("BW_CAT", c("BIGBABY1"))

# PLANNEDFEED ----
fig21 <- fig_compile_2(c('PLANNEDFEEDC1','PLANNEDFEEDC2',
                         'PLANNEDFEEDC3')) %>% subset(Var != "Not selected")
fig21 <- fig_compile("PLANNEDFEED_ONLY")

# Preg weight gain ----
# PREGWEIGHT - PREPREGWEIGHT
fig22 <- print.fig("WEIGHTGAIN")

# CHAPTER 2 ####
# DUE DATE & BIRTHDATE ----
fig23 <- print.cont("xGESTAGE") #GESTAGE is off

# GOLDENHOUR ----
fig24 <- fig_compile('GOLDENHOUR', c("RACE", "MODE2023"))



# SKIN ----
fig25a <- LTM_dsn %>% 
  subset(GOLDENHOUR %in% c("In my arms of on my chest", 
                           "In arms or on chest of my partner/spouse")) %>%
  group_by(SKIN) %>% 
  summarize(prop = survey_prop()) %>%
  select(-c(prop_se))

fig25b <- LTM_dsn %>% 
  subset(GOLDENHOUR %in% c("In my arms of on my chest", 
                           "In arms or on chest of my partner/spouse")) %>%
  group_by(RACE, SKIN) %>% 
  summarize(prop = survey_prop()) %>%
  select(-c(prop_se)) %>% 
  spread(key = RACE, value = prop)

fig25c <- LTM_dsn %>% 
  subset(GOLDENHOUR %in% c("In my arms of on my chest", 
                           "In arms or on chest of my partner/spouse")) %>%
  group_by(MODE2023, SKIN) %>% 
  summarize(prop = survey_prop()) %>%
  select(-c(prop_se)) %>% 
  spread(key = MODE2023, value = prop)

fig25 <- merge(fig25a, merge(fig25b, fig25c))

# BIRTHATTEND ----
fig26 <- fig_compile('BIRTHATTEND2')

# BIRTH DOULA ----
# DOULAC2
fig27 <- fig_compile('DOULAC2')

# SDM with induction ----
fig28 <- fig_compile("INDUCE5")

fig29a <- fig_compile("DEC_MAKE")
fig29b <- fig_compile("INDUCE3") %>% 
  rename(DEC_MAKE = INDUCE3)

rownames(fig29a) <- NULL
rownames(fig29b) <- NULL

fig29 <- rbind(fig29a, fig29b)

# Self Induce ----
fig30 <- fig_compile_2(c("SELFINDUCE2C1", "SELFINDUCE2C2",
                         "SELFINDUCE2C3","SELFINDUCE2C4",
                         "SELFINDUCE2C5","SELFINDUCE2C6",
                         "SELFINDUCE2C7","SELFINDUCE2C8")) %>%
  subset(Var != "Not selected")

# Methods in MEDINDUCE =1 ----
# MEDINDUCE	Did your maternity care provider try to induce your labor in any way? That is, use medicine or some other method to try to start regular labor contractions – before they started on their own?
# MEDINDUCE1C1	  1 = Break your bag of water with a small tool like a crochet hook (before labor)
# MEDINDUCE1C2	  2 = Insert a finger into your cervix to “sweep” or “strip” the membranes loose
# MEDINDUCE1C3	  3 = Put an inflatable bulb in your cervix (Foley balloon)
# MEDINDUCE1C4	  4 = Give you Pitocin (“pit” or synthetic oxytocin) through an intravenous (IV) line (before labor)
# MEDINDUCE1C5	  5 = Place medicine (gel, pouch, tablet) near your cervix
# MEDINDUCE1C6	  6 = Give you a tablet by mouth
# MEDINDUCE1C7	  7 = Direct you to try non-medical approaches (for example walking, castor oil, etc)
# MEDINDUCE1C8	  98 = I’m not sure
# MEDINDUCE1C9	  99 = I’d prefer not to answer
fig31 <- fig_compile_2(c('MEDINDUCE1C1','MEDINDUCE1C2','MEDINDUCE1C3',
                         'MEDINDUCE1C4','MEDINDUCE1C5','MEDINDUCE1C6',
                         'MEDINDUCE1C7','MEDINDUCE1C8','MEDINDUCE1C9'))%>%
  subset(Var != "Not selected")

# MEDINDUCE3 | MEDINDUCE = 1 ----

fig32 <- fig_compile_2(c('MEDINDUCE3C1','MEDINDUCE3C2','MEDINDUCE3C3',
                         'MEDINDUCE3C4','MEDINDUCE3C5','MEDINDUCE3C6',
                         'MEDINDUCE3C7','MEDINDUCE3C8')) %>%
  subset(Var != "Not selected")


# Position | Mode = 1 ----
fig33 <- fig.2by2("POSITIONCHOICE", "POSITION",
                  data = subset(LTM_dsn, MODE2023 == "Vaginal birth")) 
fig34 <- fig_compile_2(c('DRUGFREEC1','DRUGFREEC2','DRUGFREEC3',
                         'DRUGFREEC4','DRUGFREEC5','DRUGFREEC6',
                         'DRUGFREEC7','DRUGFREEC8','DRUGFREEC9',
                         'DRUGFREEC10','DRUGFREEC11'), 
                       others = c("DOULA", "PARITY", "BIRTHATTEND2")) %>%
  subset(Var != "Not selected")
fig35 <- fig34

# Painmeds ----

fig36 <- fig_compile_2(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
                         "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
                         "PAINMEDSC7")) %>%
  subset(Var != "Not selected")
fig37 <- fig36

# Labor int ----
fig38 <- fig_compile_2(c('LABORINTC1','LABORINTC2',
                         'LABORINTC3','LABORINTC4')) %>%
  subset(Var != "Not selected")

# position ----
fig39 <- fig_compile('POSITION')

# physiologic childbirth ----

fig40 <- fig_compile('phys_cb', c("BIRTHATTEND2", "DOULAC2"))

fig41 <- data.frame()
# hospfeed ----
fig42 <- fig_compile_2(c('HOSPFEEDC1','HOSPFEEDC2','HOSPFEEDC3','HOSPFEEDC4',
                         'HOSPFEEDC5','HOSPFEEDC6','HOSPFEEDC7',
                         'HOSPFEEDC9','HOSPFEEDC10'), c("FEED1WEEKC1")) %>%
  subset(Var != "Not selected")

# Subscale respectful care ----
fig43 <- fig_compile("NPCMC_SC")

# culture ----
fig44 <- fig_compile("CUSTOMS") 

# CHAPTER 3 ####
# Mode of Birth ----
fig45 <- fig_compile('MODE2023')

# Unplanned Reason | CSECTIONTYPE = 2 ----
fig46 <- fig_compile('UNPLANNEDREASON')

# Planned C reason ----
# PLANNEDC
fig47 <- fig_compile('PLANNEDC')

# VBAC Rate ----
fig48 <- fig_compile('xMODE2')

# Reason for Repeat CS ----
fig49 <- fig47

# CHAPTER 4 ####
# DOULA ----
fig50 <- fig_compile('DOULAC3')

# PPVISIT ----
# PPVISIT categorized as 0, 1, 2, 3, 4+ 
fig51 <- print.fig('PPVISIT2')

# PLANNEDFEED vs. FEED1WEEK ----
fig52 <- fig_compile("FEED_CONCORDANT")

# HOSPFEED ----
fig53 <- fig_compile("FEED_CONCORDANT", c("BIRTHATTEND2", "SUM_HOSPFEED", "BFGOAL"))
fig54 <- fig53

# EXCLBFGOAL ----
# EXCLBFGOAL
fig55 <- fig_compile('EXCLBFGOAL')

# BFGOAL ----
fig56 <- fig_compile('BFGOAL')

# Depression ----

fig57a <- fig_compile('PHQ4_PREG_DEP')%>% 
  rename(PHQ4 = PHQ4_PREG_DEP) %>% 
  mutate(PHQ4 = c("No PREG DEP", "Yes PREG DEP"))

fig57b <- fig_compile('PHQ4_PPDEP')%>% 
  rename(PHQ4 = PHQ4_PPDEP) %>% 
  mutate(PHQ4 = c("No PP DEP", "Yes PP DEP"))
rownames(fig57a) <- NULL
rownames(fig57b) <- NULL
fig57 <- rbind(fig57a, fig57b)

# Anxiety ----

fig58a <- fig_compile('PHQ4_PREG_ANX') %>% 
  rename(PHQ4 = PHQ4_PREG_ANX) %>% 
  mutate(PHQ4 = c("No PREG ANX", "Yes PREG ANX"))

fig58b <- fig_compile('PHQ4_PPANX') %>% 
  rename(PHQ4 = PHQ4_PPANX) %>% 
  mutate(PHQ4 = c("No PP ANX", "Yes PP ANX"))
rownames(fig58a) <- NULL
rownames(fig58b) <- NULL
fig58 <- rbind(fig58a, fig58b)

# Psych distress
fig59a <- fig_compile('PHQ4_PREG_PSYCH')%>% 
  rename(PHQ4 = PHQ4_PREG_PSYCH) %>% 
  mutate(PHQ4 = c("Mild PREG PSYCH", "Moderate PREG PSYCH", 
                  "Normal PREG PSYCH", "Severe PREG PSYCH")) 

fig59b <- fig_compile('PHQ4_PPPSYCH')%>% 
  rename(PHQ4 = PHQ4_PPPSYCH) %>% 
  mutate(PHQ4 = c("Mild PP PSYCH", "Moderate PP PSYCH", 
                  "Normal PP PSYCH", "Severe PP PSYCH")) 
rownames(fig59a) <- NULL
rownames(fig59b) <- NULL
fig59 <- rbind(fig59a, fig59b)

# PPMEDS ----
fig60 <- fig_compile('PPTHERAPY', c("PHQ4_PPPSYCH", "PHQ4_PREG_PSYCH", "RACE", "INSURANCE"))

# Social needs resolved & unresolved ----
fig61 <- data.frame() 
for(i in c("SNMEAL","SNLIVE", "SNUTILITIES",
           "SNTRANSPORT","SNCHILDCARE", "SNINCOME",
           "SNDRUGS", "SNUNSAFE", "SNABUSE")){
  t <- fig_compile(i) %>% as.data.frame
  if("MENA" %in% colnames(t)){
    rownames(t) <- NULL
    colnames(t)[1] <- "Var"
  } else {
    t <- t %>% 
      mutate(MENA = c(NA, NA)) %>% 
      select(c(i, "prop", "AIAN-NHPI", "Asian", "Black", "Latine", "MENA", 
               "Multi", "White", "Private", "Medicaid", "Metropolitan", 
               "Nonmetropolitan"))
    rownames(t) <- NULL
    colnames(t)[1] <- "Var"
  }
  
  fig61 <- rbind(fig61, t)
}

fig61$ITEM <- rep(c("SNMEAL","SNLIVE", "SNUTILITIES",
                    "SNTRANSPORT","SNCHILDCARE", "SNINCOME",
                    "SNDRUGS", "SNUNSAFE", "SNABUSE"), each = 2)

fig62 <- fig_compile("SUM_SNNEEDS")

# Other variables ----
# limited to multips, 
# CURREDUC differed by having taken classes in the past

LTM_dsn %>% 
  filter(PRIOREDUC %in% c("No", "Yes")) %>%
  # filter(CURREDUC %in% c("No", "Yes")) %>%
  filter(NUMB_BIRTH > 1) %>% 
  tbl_svysummary(by = PRIOREDUC, 
                 include = c(CURREDUC))

LTM_dsn %>% 
  tbl_svysummary(include = CLASS_ANY)

LTM_dsn %>% 
  # filter(CARETYPE1 == "No") %>% 
  tbl_svysummary(include = CAREMODE_R)

LTM_dsn %>% 
  # filter(PREPREG_MHCONDC1 == "Depression or sadness") %>% 
  tbl_svysummary(by = PREPREG_MHCONDC1, 
                 include = PHQ4_PREG_DEP) %>% add_p()

LTM_dsn %>% 
  tbl_svysummary(by = PREPREG_MHCONDC2, 
                 include = PHQ4_PREG_ANX) %>% add_p()

LTM_dsn %>% 
  tbl_svysummary(by = PREPREG_MHCONDC2, 
                 include = PHQ4_PREG_ANX) %>% add_p()

LTM_dsn %>% 
  tbl_svysummary(by = INCCAT, 
                 include = SUM_SOCIALNEED, 
                 statistic = all_continuous() ~ "{min}, {p25},{median},{p75},{max}")

LTM_dsn %>% 
  tbl_svysummary(by = PREPREG_MHANY, 
                 include = MEDSANY) %>% 
  add_p()

LTM_dsn %>% 
  filter(BIGBABY2 %in% c("Yes, a labor induction",
                         "Yes, a C-section",
                         "Yes, something else (please specify)")) %>%
  tbl_svysummary(by = MEDINDUCE, 
                 include = c(xGESTAGE)) %>% 
  add_ci()

LTM_dsn %>% 
  filter(BIGBABY2 == "Yes, a labor induction") %>% 
  tbl_svysummary(#by = MEDINDUCE , 
    include = c(MEDINDUCE),
    statistic = list(all_categorical() ~ "{p}%", 
                     all_continuous() ~ "{mean}, {sd}")) %>% 
  add_ci()

LTM_dsn %>% 
  tbl_svysummary(by = BIGBABY1, 
                 include = c(#GESTAGE_R, 
                   GESTAGE_R_cont), 
                 statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")) %>% 
  add_ci()


LTM_dsn %>% 
  tbl_svysummary(by = YEARBIRTH, 
                 include = WHYTELEC4)
#espondents with private health insurance (average x weeks) learned about their 
# pregnancies earlier than those with Medicaid/CHIP (average y weeks) 
# (p < 0.05). Respondents with first births (average x weeks) 
# learned about their pregnancies earlier than those with prior 
# births (average y weeks) (p < 0.05). 

LTM_dsn %>% 
  tbl_svysummary(by = INSURCAT, 
                 include = LEARNED1, 
                 statistic = list(all_continuous() ~ "{mean}")) %>% add_p()

# List of datasets ----
list_figs <- setNames(
  mget(paste0("fig", 1:61)),
  paste0("fig", 1:61))

# Final Export ----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Results")
write.xlsx(list_figs, file = "AllFigures.xlsx")
