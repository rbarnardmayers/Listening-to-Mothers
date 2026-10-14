# 

LTM_CA <- LTM_final %>%
  select(UID2, RACEC8,RACEC7,RACEC6,RACEC5,RACEC4,RACEC3,RACEC2,RACEC1) %>%
  mutate(UID2 = as.character(UID2))

NPWF_LTM_California_Filtered_Open_Ends <- NPWF_LTM_California_Filtered_Open_Ends %>% 
  left_join(LTM_CA)

setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/California")
write.csv(NPWF_LTM_California_Filtered_Open_Ends, "Q2_OEs.csv")

