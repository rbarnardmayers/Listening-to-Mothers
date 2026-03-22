# Setup ----
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Data Cleaning Chapter 6.R")

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

# Table Runs ----
# Impact whiteness 
for(i in comp_cols){
  k = match(i, comp_cols)
  dat <- fig.2by2.bases(var1 = "IMPACT_WHITE",var_name = i)
  assign(paste0("tab_white_", k),dat)
}

# Impact Black, 
for(i in comp_cols){
  k = match(i, comp_cols)
  dat <- fig.2by2.bases(var1 = "IMPACT_BLACK",var_name = i)
  assign(paste0("tab_black_", k),dat)
}

# ALONE RESULTS 
for(i in comp_cols){
  k = match(i, comp_cols)
  dat <- fig.2by2.bases(var1 = "RACEALONE",var_name = i)
  assign(paste0("tab_alone_", k),dat)
}

# Multi comparison 
for(i in comp_cols){
  k = match(i, comp_cols)
  row1 <- get(paste0("tab_alone_", k)) %>% 
    subset(RACEALONE == "Black or African American ALONE") %>%
    rename(CATEGORY = RACEALONE)
  row2 <- fig.2by2.bases(var1 = "MULTIRACIAL1",var_name = i) %>% 
    subset(MULTIRACIAL1 == "Multiracial")%>%
    rename(CATEGORY = MULTIRACIAL1)
  row3 <- get(paste0('tab_black_', k)) %>% 
    subset(IMPACT_BLACK == "MULTI_NOBLACK")%>%
    rename(CATEGORY = IMPACT_BLACK)
  row4 <- get(paste0('tab_white_', k)) %>% 
    subset(IMPACT_WHITE == "MULTI_NOWHITE")%>%
    rename(CATEGORY = IMPACT_WHITE)
  
  dat <- rbind(row1, row2, row3, row4)
  assign(paste0("tab_multi_", k),dat)
}

# AFROLATINE/LATINEWHITE
for(i in comp_cols){
  k = match(i, comp_cols)
  row1 <- get(paste0("tab_alone_", k)) %>%
    subset(RACEALONE == "Black or African American ALONE") %>%
    rename(CATEGORY = RACEALONE)
  row2 <- fig.2by2.bases(var1 = "AFROLATINE",var_name = i) %>%
    subset(AFROLATINE == "Black + Latine")%>%
    rename(CATEGORY = AFROLATINE)
  row3 <- get(paste0("tab_alone_", k)) %>%
    subset(RACEALONE == "Latine ALONE") %>%
    rename(CATEGORY = RACEALONE)
  row4 <- fig.2by2.bases(var = "LATINEWHITE", var_name = i) %>%
    subset(LATINEWHITE != "Other") %>%
    rename(CATEGORY = LATINEWHITE)
  row5 <- get(paste0("tab_alone_", k)) %>% 
    subset(RACEALONE == "White ALONE") %>%
    rename(CATEGORY = RACEALONE)
  
  dat <- rbind(row1, row2, row3, row4, row5)
  assign(paste0("tab_aflat_", k),dat)
}


# AIAN-NHPI-ASIAN
for(i in comp_cols){
  k = match(i, comp_cols)
  row1 <- get(paste0("tab_alone_", k)) %>% 
    subset(RACEALONE == "American Indian or Alaskan Native ALONE") %>%
    rename(CATEGORY = RACEALONE)
  row2 <- get(paste0("tab_alone_", k)) %>% 
    subset(RACEALONE == "Native Hawaiian or Pacific Islander ALONE") %>%
    rename(CATEGORY = RACEALONE)
  row3 <- get(paste0("tab_alone_", k)) %>% 
    subset(RACEALONE == "Asian ALONE") %>%
    rename(CATEGORY = RACEALONE)
  
  dat <- rbind(row1, row2, row3)
  assign(paste0("tab_asian_", k),dat)
}
# Exporting ----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Results")

list_figs_white <- setNames(
  mget(paste0("tab_white_", 1:107)),
  comp_cols)
write.xlsx(list_figs_white, file = "WhiteImpact_results.xlsx")

list_figs_black <- setNames(
  mget(paste0("tab_black_", 1:107)),
  comp_cols)
write.xlsx(list_figs_black, file = "BlackImpact_results.xlsx")

list_figs_multi <- setNames(
  mget(paste0("tab_multi_", 1:107)),
  comp_cols)
write.xlsx(list_figs_multi, file = "Multi_results.xlsx")

list_figs_afrol <- setNames(
  mget(paste0("tab_aflat_", 1:107)),
  comp_cols)
write.xlsx(list_figs_afrol, file = "Afro_results.xlsx")

list_figs_asian <- setNames(
  mget(paste0("tab_asian_", 1:107)),
  comp_cols)
write.xlsx(list_figs_asian, file = "Asian_results.xlsx")

