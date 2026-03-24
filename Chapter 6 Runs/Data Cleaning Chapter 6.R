# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.r")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Data Cleaning.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")


LTM_6 <- LTM_final %>% 
  mutate(MULTI_NOBLACK = case_when(RACE == "Multi" & 
                                     RACEC3 == "Not selected" ~ "MULTI_NOBLACK",
                                   RACEC3 != "Not selected" ~ "BLACK"),
         MULTI_NOWHITE = case_when(RACE == "Multi" & 
                                     RACEC1 == "Not selected" ~ "MULTI_NOWHITE", 
                                   RACEC1 != "Not selected" ~ "White"),
         WHITECOMBI_NOBLACK = case_when(RACECOMBI1 == "White in combination" & 
                                          RACEC3 == "Not selected" ~ "WHITECOMBI_NOBLACK"),
         BLACKCOMBI_NOWHITE = case_when(RACECOMBI3 == "Black or African American in combination" & 
                                          RACEC1 == "Not selected" ~ "BLACKCOMBI_NOWHITE"),
         IMPACT_WHITE = case_when(RACEALONE == "White ALONE" ~ "WHITEALONE",
                                  BLACKWHITE == "Black + White" ~ "BLACKWHITE",
                                  WHITECOMBI_NOBLACK == "WHITECOMBI_NOBLACK" ~ "WHITECOMBI_NOBLACK",
                                  MULTI_NOWHITE == "MULTI_NOWHITE" ~ "MULTI_NOWHITE"),
         IMPACT_WHITE = factor(IMPACT_WHITE, levels = c("WHITEALONE", "BLACKWHITE", 
                                                        "WHITECOMBI_NOBLACK", "MULTI_NOWHITE")),
         IMPACT_BLACK = case_when(RACE == "Black" ~ "BLACKALONE",
                                  BLACKWHITE == "Black + White" ~ "BLACKWHITE",
                                  BLACKCOMBI_NOWHITE == "BLACKCOMBI_NOWHITE" ~ "BLACKCOMBI_NOWHITE",
                                  MULTI_NOBLACK == "MULTI_NOBLACK" ~ "MULTI_NOBLACK"),
         IMPACT_BLACK = factor(IMPACT_BLACK, levels = c("BLACKALONE", "BLACKWHITE", 
                                                        "BLACKCOMBI_NOWHITE", "MULTI_NOBLACK")),
  )

