# TO DO ----
library(readxl)
LTM_raw <- read_excel("~/Desktop/LTM Original Data/Q1 Unweighted.xlsx")


# Results ----
# Run report 2 code but comment out line 7 (subset(MDID2))
LTM_final <- LTM_final %>% 
  mutate(WHITE_HISP = case_when(RACEC1 == "White" & 
                                  RACEC2 == "Hispanic or Latina" ~ 'White+Hisp', 
                                TRUE ~ "Not"), 
         WHITE_HISP_ONLY = case_when(RACEC1 == "White" & 
                                       RACEC2 == "Hispanic or Latina" & 
                                       RACEC3 == "Not selected" & 
                                       RACEC4 == "Not selected" & 
                                       RACEC5 == "Not selected" & 
                                       RACEC6 == "Not selected" & 
                                       RACEC7 == "Not selected" ~ 'White+Hisp Only', 
                                     TRUE ~ "Not"), 
         BLACKWHITE_ANY = case_when(RACEC1 == "White" & 
                                      RACEC3 == "Black or African American" ~ "Black+White", 
                                    TRUE ~ "Not"))

LTM_dsn <- LTM_final %>%
  as_survey_design(weight = FINALWT_q1_q2, id = 1)

# BlackMoms Report surveys ----
# White
table(LTM$which_surv, LTM$RACEC1)[,2]
# Latina 
table(LTM$which_surv, LTM$RACEC2)[,2]
# Black
table(LTM$which_surv, LTM$RACEC3)[,2]
# Asian
table(LTM$which_surv, LTM$RACEC4)[,2]
# AIAN-NHPI
table(LTM$which_surv, LTM$AIAN_NHPI)[,1]

# Alones
table(LTM$which_surv, LTM$RACEALONE)


# For Q1 text ----
r_svysummary(include = "PLANNED_INDUCED",
             data = filter(LTM_dsn, RACEALONE == "Black or African American ALONE"))

r_svysummary(include = "PLANNED_INDUCED",
             data = filter(LTM_dsn, RACEC3 == "Black or African American" & 
                             is.na(RACEALONE)))
# FOR Q1 data ----
# Figure 2
# RACECOMBI3 == "Black or African American in combination"
# RACEALONE == "Black or African American ALONE"
# RACEC3 == "Black or African American"
# RACE
r_svysummary(by = "RACECOMBI3", 
             include = "CUM_INT")
# data = filter(LTM_dsn, RACECOMBI3 == "Black or African American in combination")

# Figure 3
r_svysummary(include = c('RESPECT', 'KNOWLEDGE', 'HEARD',
                         'DECISIONS', 'CONSENT', 'INFORMED',
                         'TRUST', 'FEEDING', 'SAFE', 'TIMELINESS',
                         'DISCRIMINATION', 'NEGLECT'),
             data = filter(LTM_dsn, RACEALONE == "Black or African American ALONE"))

# Figure 4 
# White Alone RACE == "White"
# White + Black BLACKWHITE == "Black + White"
# Black in Combination (no white) RACECOMBI3 == "Black or African American in combination" & RACEC1 == "Not selected"
# Black alone RACE == "Black"

# PREPREG_MHCONDC2 PHQ4_PREG_ANX PHQ4_PPANX 
r_svysummary(include = c('PREPREG_MHCONDC2', 'PHQ4_PREG_ANX', 'PHQ4_PPANX' ),
             data = filter(LTM_dsn, 
                           BLACKWHITE == "Black + White"))

# PREPREG_MHCONDC1 PHQ4_PREG_DEP PHQ4_PPDEP 
r_svysummary(include = c('PREPREG_MHCONDC1', 'PHQ4_PREG_DEP', 'PHQ4_PPDEP' ),
             data = filter(LTM_dsn, 
                           RACEC3 == "Black or African American"))

# Figure 5
# RACEC3 == "Black or African American"
r_svysummary(include = c('SOCIALNEEDC1', 'SOCIALNEEDC2', 'SOCIALNEEDC3', 
                         'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                         'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                         'SOCIALNEEDC10'),
             data = filter(LTM_dsn, 
                           RACEC3 == "Black or African American"))

# FOR Q2 data ----
# RACEC3 == "Black or African American"
# Black alone RACE == "Black"
# RACECOMBI3 == "Black or African American in combination"
raw_svysummary(#by = "RACECOMBI3", 
             include = c("FUTUREBABYDIFF1_A1", "FUTUREBABYDIFF1_A2",
                         "FUTUREBABYDIFF1_A3", "FUTUREBABYDIFF1_A4"), 
             data = filter(LTM_dsn, FUTUREBIRTH != '0'& 
                             FUTUREBIRTH != "I'd prefer not to answer"))



# CROSS SURVEY ANALYSIS ----
# EMPLOY EMPLOYBEN EMPLOYCAT INSUR 
r_svysummary(by = "RACE",
             include = c('EMPLOY',
                         'EMPLOYBEN',
                         'EMPLOYCAT',
                         'INSURANCE'))

r_svysummary(by = "RACE",
             include = c('EMPLOYCAT'), 
             data = filter(LTM_dsn, EMPLOYCAT != "Not employed"))


# For Appendix  ----
# run apply dictionary but comment out rows 6 and 7 in data_cleaning_1 
# change lines 52 and 55 to FINALWT_q1_q2

r_svysummary(include = c("AGE4", "INSURCAT", "EDUC",
                         "DELIVERY_TYPE", "NUM_BIRTH1",
                         "BIRTHCOUNTRYUS", "DISABILITY", 
                         "PARITY", "INCCAT", "URBANICITY2"), 
             data = filter(LTM_dsn, RACE == "Black"))


r_svysummary(include = c("AGE4", "INSURCAT", "EDUC",
                         "DELIVERY_TYPE", "NUM_BIRTH1",
                         "BIRTHCOUNTRYUS", "DISABILITY", 
                         "PARITY", "INCCAT", "URBANICITY2"), 
             data = filter(LTM_dsn, RACE == "Multi" & 
                             RACEC3 == "Black or African American"))

r_svysummary(include = c("RACEC1", 'RACEC2', 'RACEC3', 
                         'RACEC4', 'RACEC5', 'RACEC6',
                         'RACEC7'),
             data = filter(LTM_dsn, RACE == "Multi"))
r_svysummary(include = c("RACEC1", "RACEC2", "RACEC3", "RACEC4", 
                         "RACEC5", "RACEC6", "RACEC7"),
             data = filter(LTM_dsn, RACE == "Multi"))

r_svysummary(include = c("WHITE_HISP", "WHITE_HISP_ONLY", "BLACKWHITE", "BLACKWHITE_ANY"),
             data = filter(LTM_dsn, RACE == "Multi"))

# Across all questionnairres ----
# unweighted_svysummary
unweighted_svysummary(include = c("AGE4", "BIRTHCOUNTRY", "PARITY", 
                          "EDUCAT", "INSURCAT", "LEARNED2_TRI",
                          "BIRTHATTEND", "MODE1INDEX", 
                          "MODE2INDEX", "ASSISTED",
                         "GESTAGE_TERM",
                         "BW_CAT", "AROM_ANY", 
                         "DISABILITY", "INCCAT", "URBANICITY2"), 
             data = filter(LTM_dsn, RACE == "Black"))

# DATA CHECKING ----
r_svysummary(include = "LABCSEC", 
             data = filter(LTM_dsn, RACE == "Black" & CSECTIONTYPE == "Unplanned"))

r_svysummary(by = "RACE", 
             include = "LABCSEC", 
             data = filter(LTM_dsn, CSECTIONTYPE == "Unplanned"))



