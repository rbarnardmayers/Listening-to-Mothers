setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Open Ends")

types <- read.csv("LTM_CSTYPE_R.csv")
unplanned <- read.csv("LTM_UNPLANNED_R.csv")
planned <- read.csv("LTM_PLANNED_R.csv")

LTM2 <- LTM2 %>% 
  full_join(types) %>% 
  full_join(planned) %>% 
  full_join(unplanned) %>% 
  mutate(CSECTIONTYPE = case_when(CSECTIONTYPE == 95 ~ CSECTIONTYPE_M,
                                  TRUE ~ CSECTIONTYPE),
         UNPLANNEDREASON = case_when(UNPLANNEDREASON == 95 ~ UNPLANNEDREASON_M, 
                                     TRUE ~ UNPLANNEDREASON),
         PLANNEDC = case_when(PLANNEDC == 95 ~ PLANNEDC_M, 
                              TRUE ~ PLANNEDC)#,
         # CSECTIONTYPE_R = case_when(CSECTIONTYPE == 2 ~ "Unplanned C-section", 
         #                            CSECTIONTYPE == 1 | CSECTIONTYPE == 95 ~ "Not Unplanned C-section",
         #                            MODE2023 == 1 ~ "Vaginal birth"), 
         # CSECTIONTYPE_R2 = case_when(CSECTIONTYPE == 2 ~ "Unplanned C-section", 
         #                             CSECTIONTYPE == 1 ~ "Planned C-section",
         #                             CSECTIONTYPE == 95 ~ "Other",
         #                             MODE1INDEX == 1 ~ "Vaginal birth")
         )

# Exporting ----
# LTM_CSTYPE <- LTM_final %>% 
#   select(c(MDID, CSECTIONTYPE, CSECTIONTYPEO)) %>% 
#   subset(CSECTIONTYPEO != "")
# 
# LTM_UNPLANNED <- LTM_final %>% 
#   select(c(MDID, UNPLANNEDREASON, UNPLANNEDREASONO)) %>% 
#   subset(UNPLANNEDREASONO != "")
# 
# LTM_PLANNED <- LTM_final %>% 
#   select(c(MDID, PLANNEDC, PLANNEDCO)) %>% 
#   subset(PLANNEDCO != "")

# write.csv(LTM_CSTYPE,"LTM_CSTYPE.csv")
# write.csv(LTM_UNPLANNED, "LTM_UNPLANNED.csv")
# write.csv(LTM_PLANNED,"LTM_PLANNED.csv")
