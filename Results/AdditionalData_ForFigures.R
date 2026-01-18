source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")

LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  tbl_svysummary(include = c(MEDINDUCE))

LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  tbl_svysummary(by = MEDINDUCE, include = c(PAINMEDSC1))

LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  subset(MEDINDUCE == "Yes") %>% 
  # subset(MEDINDUCE == "No") %>% 
  tbl_svysummary(by = PAINMEDSC1, include = c(MODE2023))

LTM_dsn %>% 
  # subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  summarize(n = unweighted(n()))

LTM_dsn %>%
  tbl_svysummary(include = c(HOSPFEEDC1,HOSPFEEDC2,HOSPFEEDC3,
                             HOSPFEEDC4,HOSPFEEDC5,HOSPFEEDC6,
                             HOSPFEEDC7,HOSPFEEDC8,HOSPFEEDC9,
                             HOSPFEEDC10,HOSPFEEDC11, HOSPFEEDC12))

LTM_dsn %>%
  subset(INDUCE == "Yes") %>%
  tbl_svysummary(by = RACE, 
                 include = c(SDM_1, SDM_2, SDM_3, SDM_4),
                 statistic = list(all_categorical() ~ "{p}"),
                 digits = list (SDM_1 ~ 3,
                                SDM_2 ~ 3,
                                SDM_3 ~ 3,
                                SDM_4 ~ 3))

LTM_dsn %>%
  subset(INDUCE == "Yes" & DEC_MAKE != "Missing") %>%
  tbl_svysummary(by = RACE, 
                 include = c(DEC_MAKE),
                 statistic = list(all_categorical() ~ "{p}"),
                 digits = list (DEC_MAKE ~ 3))
 
LTM_dsn %>%
  subset(INDUCE == "Yes") %>%
  tbl_svysummary(by = RACE, 
                 include = c(INDUCE4, INDUCE3, INDUCE2, INDUCE1),
                 statistic = list(all_categorical() ~ "{p}"),
                 digits = list (INDUCE4 ~ 3,
                                INDUCE3 ~ 3,
                                INDUCE2 ~ 3,
                                INDUCE1 ~ 3))
LTM_dsn %>% 
  subset(xLEARNED2 != "I did not have any prenatal visits") %>% 
  subset(PROVIDERCHOICE != "Missing") %>%
  tbl_svysummary(by = RACE,
                 include = c(PROVIDERCHOICE),
                 statistic = list(all_categorical() ~ "{p}"), 
                 digits = list(PROVIDERCHOICE ~ 3))
LTM_dsn %>% 
  subset(xLEARNED2 != "I did not have any prenatal visits") %>% 
  subset(PROVIDERCHOICE != "Missing") %>%
  tbl_svysummary(#by = INSURANCE,
                 include = c(PROVIDERCHOICE),
                 statistic = list(all_categorical() ~ "{p}"), 
                 digits = list(PROVIDERCHOICE ~ 3))


LTM_dsn %>% 
  tbl_svysummary(by = RACE, 
                 include = c(DOULA, FIRSTVISIT, 
                             NOPRENATALC1),
                 digits = list(DOULA ~ 3))

