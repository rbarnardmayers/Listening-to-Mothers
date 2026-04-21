source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.r")

# Figure 4.1
# RACE, INSURANCE, PROVIDER
r_svysummary(by = "RACE", 
             include = "MODE2023")

# Figure 4.2
r_svysummary(by = "xMODE2", include = "CSECTIONTYPE")

# Figure 4.3 
# RACE, INSURANCE, PARITY, PROVIDER, BIRTHATTEND
r_svysummary(by = "RACE", 
             include = "CSECTIONTYPE_R2")

# Figure 4.4
r_svysummary(include = "UNPLANNEDREASON")

# Figure 4.5
r_svysummary(by = "SUM_LABORSTUFF", 
             include = "CSECTIONTYPE_R", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))

# Figure 4.6
r_svysummary(include = "PLANNEDC")


# Figure 4.7
# RACE, INSURANCE, PROVIDER, DOULAC1
r_svysummary(by = "RACE", 
             include = "xMODE2", 
             data = filter(LTM_dsn, xMODE2 %in% c("Cesarean Repeat",
                                                  "VBAC")))

# Figure 4.8
# Pie chart VBAC interest 
r_svysummary(include = "VBACINTEREST", 
             data = filter(LTM_dsn, 
                           xMODE2 == "Cesarean Repeat"))
r_svysummary(by = "VBACINTEREST", include = "VBACCHOICE")
r_svysummary(include = c("VBACACCESSC1", "VBACACCESSC2","VBACACCESSC3"), 
             data = filter(LTM_dsn, VBACINTEREST == "Yes" & VBACCHOICE == "No"))

# For carol 
r_svysummary(include = "NUM_CS",
            data = filter(LTM_dsn, PRIOR_C == 1))

# TABLES ----
# Table 4.1 
count_svysummary(by = "xMODE2", 
                 include = "CSECTIONTYPE")
count_svysummary(by = "xMODE2", 
                 include = "VAGASSIST")

# Table 4.2
r_svysummary(by = "SUM_LABORSTUFF", 
             include = "CSECTIONTYPE_R", 
             data = filter(LTM_dsn, MODE2023 == "Vaginal birth" | LABCSEC == "Yes"))

LTM_final %>% 
  subset(SUM_LABORSTUFF %in% c("5", "6", "7")) %>% 
   select(c(DOULAC1, DOULAC2, FETALMONC2_R, VAGEXAM_5, 
            LABORWALK_R, LABORPERMIT_A1_R, LABORPERMIT_A2_R,
            PROVIDER_R, SUM_LABORSTUFF,UNPLANNEDREASON)) %>%
  View()

# BIRTHATTEND2, DOULAC2, VAGEXAM_5, FETALMONC1, MEDINDUCE, LABORWALK

# Table 4.4 
r_svysummary(include = c("MODE2023", "xMODE2"))
r_svysummary(by = "PRIOR_C", 
             include = "MODE2023")

