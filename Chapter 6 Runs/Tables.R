# Setup ----
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Data Cleaning Chapter 6.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Function.R")

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
  dat <- data_comp_alone(i)
  assign(paste0(i, "_tab_Alone"),dat)
}  

# AFROLATINE
for(i in comp_cols){
  dat <- data_comp_AL(i)
  assign(paste0(i, "_tab_AL"),dat)
}   

# LATINEWHITE
for(i in comp_cols){
  dat <- data_comp_LW(i)
  assign(paste0(i, "_tab_LW"),dat)
}   

# Multi Comparison ----
for(i in comp_cols){
  top <- get(paste0(i, "_tab_Alone"))
  top <- top[1:3,]
  colnames(top) <- paste0("V", 1:ncol(top))
  
  row1 <- get(paste0(i, "_tab_Alone"))
  row1 <- row1[row1[,1] == "Black",]
  colnames(row1) <- colnames(top)
  
  row2 <- get(paste0(i, "_tab_AL"))
  row2 <- row2[row2[,1] == "BlackLatine",]
  colnames(row2) <- colnames(top)
  
  
  row3 <- get(paste0(i, "_tab_Alone"))
  row3 <- row3[row3[,1] == "Latine",]
  colnames(row3) <- colnames(top)
  
  row4 <- get(paste0(i, "_tab_LW"))
  row4 <- row4[row4[,1] == "WhiteLatine",]
  colnames(row4) <- colnames(top)
  
  row5 <- get(paste0(i, "_tab_Alone"))
  row5 <- row5[row5[,1] == "White",]
  colnames(row5) <- colnames(top)
  
  dat <- rbind(top, row1, row2, row3, row4, row5)
  assign(paste0(i, "_tab_Multi"),dat)
}

# Asian ----
# AIANALONE 
# NHPIALONE 
# ASIANALONE 
for(i in comp_cols){
  top <- get(paste0(i, "_tab_Alone"))
  top <- top[1:3,]
  colnames(top) <- paste0("V", 1:ncol(top))
  
  row1 <- get(paste0(i, "_tab_Alone"))
  row1 <- row1[row1[,1] == "AIAN",]
  colnames(row1) <- colnames(top)
  
  row2 <- get(paste0(i, "_tab_Alone"))
  row2 <- row2[row2[,1] == "NHPI",]
  colnames(row2) <- colnames(top)
  
  row3 <- get(paste0(i, "_tab_Alone"))
  row3 <- row3[row3[,1] == "Asian",]
  colnames(row3) <- colnames(top)
  
  dat <- rbind(top, row1, row2, row3)
  assign(paste0(i, "_tab_Asian"),dat)
}


# Exporting -----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Chapter 6 Runs/Results")
# Impact White
IW_names <- ls(pattern = "_tab_IW")
IW_list <- mget(IW_names)
names(IW_list) <- comp_cols
write_xlsx(IW_list, path = "WhiteImpact_results.xlsx")

# Impact Black
IB_names <- ls(pattern = "_tab_IB")
IB_list <- mget(IB_names)
names(IB_list) <- comp_cols
write_xlsx(IB_list, path = "BlackImpact_results.xlsx")

# Multi
MUL_names <- ls(pattern = "_tab_Multi")
MUL_list <- mget(MUL_names)
names(MUL_list) <- comp_cols
write_xlsx(MUL_list, path = "Multi_results.xlsx")

# Asian results
AS_names <- ls(pattern = "_tab_Asian")
AS_list <- mget(AS_names)
names(AS_list) <- comp_cols
write.xlsx(AS_list, file = "Asian_results.xlsx")

