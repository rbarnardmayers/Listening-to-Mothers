source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")

# Table 2.1: PERCEIVEDRACE
r_svysummary(include = "PERCEIVEDRE_CONC")

r_svysummary(#by = "RACE_R", 
  include = c("PERCEIVEDRE_CONC", 
              "PERCEIVEDRE_NOT"))

# Table 3.1: MORB
# data = filter(LTM_dsn, 
#               MORB_A25 %in% c("A major new problem", 
#                               "A minor new problem") & 
#                 TIME_SINCE_BIRTH >= 24)

r_svysummary(include = c("MORB_A1", "MORB_A2"), 
             data = filter(LTM_dsn, 
                           MODE1INDEX == "Vaginal all"))

r_svysummary(include = c("MORB_A3", "MORB_A4"), 
             data = filter(LTM_dsn,
                           MODE1INDEX == "Cesarean all"))

r_svysummary(include = c(paste0("MORB_A", c(5:6,10:20))))

r_svysummary(include = c('MORB_A7', 'MORB_A8', "MORB_A9"), 
             data = filter(LTM_dsn, 
                           FEED1WEEKC1 == "Breast milk"))


count_svysummary(include = "MORBPERSIST_A20_R", 
                 data = filter(LTM_dsn, 
                               MORB_A20 %in% c("A major new problem", 
                                               "A minor new problem") & 
                                 TIME_SINCE_BIRTH >= 24))

# TABLE 3.CURRFEED
r_svysummary(by = "TIME_SINCE_BIRTH_HALF_2", 
             include = c("CURRFEEDC1", 
                         "CURRFEEDC2",
                         "CURRFEEDC3", 
                         "CURRFEEDC4"))


# Table 4.preg status
r_svysummary(include = "EMPLOY", 
             data = filter(LTM_dsn, Q2FIELD != "Standalone"))

r_svysummary(by = "EMPLOY", 
             include = "EMPLOYCAT", 
             data = filter(LTM_dsn, Q2FIELD != "Standalone"))


r_svysummary(include = "EMPLOYCAT", 
             data = filter(LTM_dsn, Q2FIELD != "Standalone" & 
                             EMPLOYCAT != "Not employed"))

# CURREMPLOY
r_svysummary(by = "EMPLOY",
             include = "CURREMPLOY")

# Table 4.MATLEAVE
r_svysummary(by = "MATLEAVEC5",
             include = c("MATLEAVE_SUM2"),
             data = filter(LTM_dsn, EMPLOY == "Yes, by someone else" & 
                             MATLEAVEC8 == "Not selected"))

r_svysummary(by = "MATLEAVE_ANY",
             include = c("MATLEAVE_SUM2"),
             data = filter(LTM_dsn,
                           EMPLOY == "Yes, by someone else"))

r_svysummary(by = "MATLEAVE_ANY",
             include = c("DEPLETE"),
             data = filter(LTM_dsn,
                           EMPLOY == "Yes, by someone else"))

# Table 4.partemploy
r_svysummary(include = "PARTEMPLOY1_R", 
             data = filter(LTM_dsn, MARRIED != "Separated/Single" & 
                             Q2FIELD != "Standalone"))

r_svysummary(by = "PARTEMPLOY",
             include = "PARTEMPLOY1", 
             data = filter(LTM_dsn, MARRIED != "Separated/Single" & 
                             Q2FIELD != "Standalone"))

# Table 4.partemployPP
r_svysummary(include = "PARTEMPLOY", 
             data = filter(LTM_dsn, CURRREL_R != "Separated/Single"))

r_svysummary(include = "PARTCURREMPLOY", 
             data = filter(LTM_dsn, CURRREL_R != "Separated/Single"))

r_svysummary(by = "PARTEMPLOY",
             include = "PARTCURREMPLOY", 
             data = filter(LTM_dsn, CURRREL_R != "Separated/Single"))


# Table 4.PARTLEAVE
r_svysummary(by = "PARTLEAVETYPEC5",
             include = c("PARTLEAVE_SUM2"),
             data = filter(LTM_dsn, PARTLEAVE1 %in% c("Completely paid",
                                                      "My spouse or partner used both paid and unpaid leave") & 
                             PARTCURREMPLOY == "Yes, by someone else" & 
                             PARTLEAVETYPEC8 == "Not selected"))

r_svysummary(by = "PARTLEAVE_ANY",
             include = c("PARTLEAVE_SUM2"),
             data = filter(LTM_dsn, PARTCURREMPLOY == "Yes, by someone else"))
# Table 4.CHALLENGEWORK
r_svysummary(include = "CHALLENGEWORK_A8", 
             data = filter(LTM_dsn, FTPTWORK_any == "Employed" & 
                             CHALLENGEWORK_A8 %in% c("A major challenge",
                                                     "A minor challenge",
                                                     "Not a challenge")))
LTM_final %>% filter(FTPTWORK_any == "Employed" & 
                       CHALLENGEWORK_A8 %in% c("A major challenge",
                                               "A minor challenge",
                                               "Not a challenge")) %>% nrow()

# Table 5.COSTMOM
svymean(~COSTMOM, filter(LTM_dsn, BILLED_MOM_0 == "> $0"), na.rm = T)
svymean(~COSTMOM1, filter(LTM_dsn, BILLED_MOM_0 == "> $0"), na.rm = T)

r_svysummary(include = c("COSTMOM_R", "COSTMOM1_R", "COSTMOM_IDK", "PAIDMOM_IDK"),
             data = filter(LTM_dsn, INSURANCE == "Private" )) 

r_svysummary(include = c("COSTMOM", "COSTMOM1"),
             data = filter(LTM_dsn, INSURANCE == "Private"  & 
                             BILLED_MOM_0 == "> $0"))

# Table 5.COSTBABY
r_svysummary(include = c("COSTBABY_R", "COSTBABY1_R", "COSTBABY_IDK", "PAIDBABY_IDK"),
             data = filter(LTM_dsn, INSURANCE == "Private" )) 

r_svysummary(include = c("COSTBABY", "COSTBABY1"),
             data = filter(LTM_dsn, INSURANCE == "Private" & 
                             BILLED_BABY_0 == "> $0"))

LTM_dsn %>%
  filter(COSTBABY > 0) %>%
  #group_by(COSTBABY_R) %>%
  summarize(median_val = survey_median(COSTBABY1, na.rm = TRUE),
            mean_val = survey_mean(COSTBABY1, na.rm = T))


# Table AVGGOOPBYINSUR
# INSURC2 - medicaid
# INSURC1 - private
# INSURC6 - none
# INSURANCE
r_svysummary(#by = "INSURC6",
  include = c("COSTMOM1"))
# c(BILLED_MOM_0 PAID_MOM_0 BILLED_BABY_0 PAID_BABY_0)
r_svysummary(#by = "INSURANCE",
  include = c('BILLED_MOM_0', 'PAID_MOM_0'))

r_svysummary(by = "BILLED_MOM_0", 
             include = "PAID_MOM_0")

# INSURBABYC2 - medicaid
# INSURBABYC1 - private
# INSURBABYC5 - none
# INSURBABY
r_svysummary(by = "INSURBABY",
             include = c("COSTBABY1", "PAID_BABY_0"))

r_svysummary(by = "PAID_BABY_0",
             include = c("COSTBABY1"), 
             data = filter(LTM_dsn, INSURBABY == "Medicaid"))

count_svysummary(#by = "INSURBABYC5",
  include = c('BILLED_BABY_0', 'PAID_BABY_0'))

#WICVALUE 
# WICVALUE_A1, WICVALUE_A2, WICVALUE_A3,
# WICVALUE_A4, WICVALUE_A5, WICVALUE_A6,
# WICVALUE_A7, WICVALUE_A8
r_svysummary(include = c("WICVALUE_A1"), 
             data = filter(LTM_dsn, 
                           WICVALUE_A1 != "I did not use this service" & 
                             WICVALUE_A1 != "I’d prefer not to answer"))

LTM_final %>% subset(WICVALUE_A7 != "I did not use this service") %>% nrow()

# COVIDVAC
# COVIDVAC 
# RACE, INSURANCE, DISABILITY, POLIT2
r_svysummary(by = "POLIT2", 
             include = c("COVIDVACC_ANY"))

# RESOURCE VALUE
r_svysummary(include = c(paste0("RESOURCEVALUE_A", 1:12), 
                         paste0("RESOURCEVALUE2_A", 1:10)))

r_svysummary(include = "RESOURCEVALUE_A1",
             data = filter(LTM_dsn, Q2FIELD == "Original Fielding"))

