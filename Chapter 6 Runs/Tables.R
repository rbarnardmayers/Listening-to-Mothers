# Setup ----
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Data Cleaning Chapter 6.R")

LTM_dsn <- LTM_6 %>% 
  mutate(FINALWT = as.numeric(FINALWT)) %>%
  as_survey_design(weight = FINALWT, id = 1)

comp_cols <- c('PREG_INT',paste0('PREPREG_MHCONDC',1:7),
               paste0('MENTALSUPPORT1C',1:6),'PROVIDER','PROVIDER2',
               'PROVIDERCHOICE','CARETYPE1','BOTHER_A1','BOTHER_A2',
               'BOTHER_A3', 'BOTHER_A4','PHQ4_PREG_ANX','PHQ4_PREG_DEP',
               'PHQ4_PREG_PSYCH','MENTALSUPPORT',paste0('PREGCONDITIONC', 1:11),
               'INDUCE','DEC_MAKE','INDUCE7','xMODE1','xMODE2','xMODE3',
               'CSECTIONTYPE','VBACCHOICE','VBACINTEREST',
               'EPIST','EPISTCHOICE','POSITIONCHOICE','RESPECT','KNOWLEDGE',
               'CUSTOMS','HEARD','DECISIONS','CONSENT','INFORMED','TIMELINESS',
               'NEGLECT','TRUST','SAFE','NPCMC_SC','DISCRIMINATION',
               paste0("DISCRIMINATION1C",1:19),'xPPVISITTIME1','PPBOTHER_A1',
               'PPBOTHER_A2','PPBOTHER_A3','PPBOTHER_A4','PHQ4_PPANX',
               'PHQ4_PPDEP','PHQ4_PPPSYCH','PPTHERAPY',
               paste0("PPMEDSC",1:6), paste0('SOCIALNEEDC',1:11))

# NEED TO FIGURE OUT OVERALL

# Impact White
for(i in comp_cols){
  dat <- data_comp_IW(i)
  assign(paste0(i, "_tab_IW"),dat)
}

# Impact Black
for(i in comp_cols){
  dat <- data_comp_IB(i)
  assign(paste0(i, "_tab_IB"),dat)
}

# Alone
for(i in comp_cols){
  dat <- data_comp_IW(i)
  assign(paste0(i, "_tab_Alone"),dat)
}  

for(i in comp_cols){
  dat <- data_comp_IW(i)
  assign(paste0(i, "_tab_IW"),dat)
}   
# Table Runs ----
overall <- r_svy6()
colnames(overall) <- c("Characteristics", "Overall", "95_Overall")
overall <- overall %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

# Impact whiteness 
impact_w <- r_svy6(by_var = "IMPACT_WHITE")
colnames(impact_w) <- c("Characteristics", 
                        "BlackWhite", "95_BW", 
                        "Multi_NoWhite", "95_MNW",
                        "White Alone", "95_WA",
                        "WhiteCombi_NoBlack", "95_WCNB")
impact_w <- impact_w %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

# Impact Black, 
impact_b <- r_svy6(by_var = "IMPACT_BLACK")
colnames(impact_b) <- c("Characteristics", 
                        "BlackAlone", "95_BA", 
                        "BlackCombi_NoWhite", "95_BCNW",
                        "BlackWhite", "95_BW",
                        "Multi_NoBlack", "95_MNB")
impact_b <- impact_b %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

# ALONE RESULTS 
impact_alone <- r_svy6(by_var = "RACEALONE")
colnames(impact_alone) <- c("Characteristics", 
                            "AIAN", "95_AIAN", 
                            'Asian', '95_Asian',
                            'Black', "95_Black", 
                            "Latine", "95_Latine", 
                            'MENA', "95_MENA",
                            "NHPI", "95_NHPI", 
                            "White", "95_White")
impact_alone <- impact_alone %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

# Multi comparison 
col_balone <- impact_alone[,c("Characteristics", "Black", "95_Black")]

col_multi <- r_svy6(by_var = "MULTIRACIAL1")
colnames(col_multi) <- c("Characteristics", "Multiracial", "95_Multi", "Other", "95_O")
col_multi <- col_multi[,c("Characteristics", "Multiracial", "95_Multi")]
col_multi <- col_multi %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

col_mnb <- impact_b[,c("Characteristics", "Multi_NoBlack", "95_MNB")]

col_mnw <-impact_w[,c("Characteristics", "Multi_NoWhite", "95_MNW")]

# AFROLATINE/LATINEWHITE
col_aflat <- r_svy6(by_var = "AFROLATINE")
colnames(col_aflat) <- c("Characteristics", "Black + Latine","95_BL", "Other", "95_O")
col_aflat <- col_aflat[,c("Characteristics", "Black + Latine", "95_BL")]
col_aflat <- col_aflat %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

col_latw <- r_svy6(by_var = "LATINEWHITE")
colnames(col_latw) <- c("Characteristics", "Other", "95_O", "White + Latine", "95_WL")
col_latw <- col_latw[,c("Characteristics", "White + Latine", "95_WL")]
col_latw <- col_latw %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

col_lat <- impact_alone[,c("Characteristics", "Latine", "95_Latine")]
col_white <- impact_alone[,c("Characteristics", "White", "95_White")]

# AIAN-NHPI-ASIAN
impact_asian <- impact_alone %>% 
  select(c("Characteristics", "AIAN", "95_AIAN", "NHPI", "95_NHPI",
           'Asian', '95_Asian'))
impact_asian <- impact_asian %>% subset(!(Characteristics %in% c("Not selected", "Unknown")))

# Exporting -----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Results")
# Impact White
IW_names <- ls(pattern = "_tab_IW")
IW_list <- mget(IW_names)
names(IW_list) <- comp_cols
write_xlsx(IW_list, path = "WhiteImpact_results.xlsx")


# Impact White
IB_names <- ls(pattern = "_tab_IB")
IB_list <- mget(IB_names)
names(IB_list) <- comp_cols
write_xlsx(IB_list, path = "BlackImpact_results.xlsx")

# 
sheets_mul <- list('BlackAlone' = col_balone, 
                   'Multi' = col_multi, 
                   'Multi_NoBlack' = col_mnb,
                   "Multi_NoWhite" = col_mnw) 

write.xlsx(sheets_mul, file = "Multi_results.xlsx")

list_figs_afrol <- list(
  "BlackAlone" = col_balone, 
  "AfroLatine" = col_aflat, 
  "LatineWhite" = col_latw, 
  "LatineAlone" = col_lat, 
  "WhiteAlone" = col_white)

write.xlsx(list_figs_afrol, file = "Afro_results.xlsx")
write.xlsx(impact_alone, file = "Alone_results.xlsx")

