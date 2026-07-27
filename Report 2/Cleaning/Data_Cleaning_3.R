source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Data_Cleaning_2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Dates2.R")

LTM3 <- LTM2

setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Others")
csfeel <- read.csv("CSECTFEEL_R.csv")
mistrust <- read.csv("MISTRUST_R.csv")
hospsafe <- read.csv("HOSPSAFE_R.csv")
lacksafe <- read.csv("LACKSAFE_R.csv")
# bcoff <- read.csv("BCOFFERED_R.csv")
counselb <- read.csv("COUNSELBARR_R.csv")
midwifeconc <- read.csv("MIDWIFECONCERN_R.csv")
# doulaconc <- read.csv("DOULACONCERN_R.csv")
Sources <- read.csv("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Sources.csv")

LTM3 <- LTM3 %>% 
  full_join(csfeel) %>%
  mutate(CSECTFEEL_POS = case_when(CSECTFEEL_POS == 1 | CSECTFEEL_POS_R == 1 ~ 1, 
                                   TRUE ~ 0),
         CSECTFEEL_NEG = case_when(CSECTFEEL_NEG == 1 | CSECTFEEL_NEG_R == 1 ~ 1, 
                                   TRUE ~ 0), 
         CSECTFEEL_ALL = case_when(CSECTFEEL_POS == 1 & CSECTFEEL_NEG == 1 ~ "Both",
                                   CSECTFEEL_POS == 1 & CSECTFEEL_NEG == 0 ~ "Positive Only",
                                   CSECTFEEL_POS == 0 & CSECTFEEL_NEG == 1 ~ "Negative Only"))

LTM3 <- LTM3 %>%
  full_join(mistrust) %>% 
  mutate(MISTRUSTC1 = case_when(MISTRUSTC1_R == 1 ~ 1, 
                                TRUE ~ MISTRUSTC1),
         MISTRUSTC3 = case_when(MISTRUSTC3_R == 1 ~ 1, 
                                TRUE ~ MISTRUSTC3),
         MISTRUSTC7 = case_when(MISTRUSTC7_R == 1 ~ 1, 
                                TRUE ~ MISTRUSTC7), 
         MISTRUSTC8 = case_when(MISTRUSTC1_R == 1 ~ 0,
                                MISTRUSTC3_R == 1 ~ 0,
                                MISTRUSTC7_R == 1 ~ 0,
                                TRUE ~ MISTRUSTC8))

LTM3 <- LTM3 %>%
  full_join(hospsafe) %>% 
  mutate(HOSPSAFEC10 = case_when(HOSPSAFEC10 == 0 & HOSPSAFEC10_R == 1 ~ 1,
                               TRUE ~ HOSPSAFEC10),
         HOSPSAFEC15 = case_when(HOSPSAFEC15 == 0 & HOSPSAFEC15_R == 1 ~ 1,
                                 TRUE ~ HOSPSAFEC15))


LTM3 <- LTM3 %>%
  full_join(lacksafe) %>% 
  mutate(LACKSAFEC1 = case_when(LACKSAFEC1 == 0 & LACKSAFEC1_R == 1 ~ 1,
                                 TRUE ~ LACKSAFEC1),
         LACKSAFEC2 = case_when(LACKSAFEC2 == 0 & LACKSAFEC2_R == 1 ~ 1,
                                 TRUE ~ LACKSAFEC2))
# LTM3 <- LTM3 %>% 
#   full_join(bcoff) %>% 
#   mutate(BCOFFEREDC1 = case_when(BCOFFEREDC1_R == 1 ~ 1, 
#                                  TRUE ~ BCOFFEREDC1))

LTM3 <- LTM3 %>%
  full_join(counselb) %>% 
  mutate(COUNSELBARRC1 = case_when(COUNSELBARRC1_R == 1 ~ 1,
                                 TRUE ~ COUNSELBARRC1),
         COUNSELBARRC2 = case_when(COUNSELBARRC2_R == 1 ~ 1,
                                 TRUE ~ COUNSELBARRC2),
         COUNSELBARRC3 = case_when(COUNSELBARRC3_R == 1 ~ 1,
                                   TRUE ~ COUNSELBARRC3),
         COUNSELBARRC4 = case_when(COUNSELBARRC4_R == 1 ~ 1,
                                   TRUE ~ COUNSELBARRC4),
         COUNSELBARRC5 = case_when(COUNSELBARRC5_R == 1 ~ 1,
                                   TRUE ~ COUNSELBARRC5),
         COUNSELBARRC7 = case_when(COUNSELBARRC7_R == 1 ~ 1,
                                   TRUE ~ COUNSELBARRC7),
         COUNSELBARRC8 = case_when(COUNSELBARRC8_R == 0 ~ 0,
                                   TRUE ~ COUNSELBARRC8))

LTM3 <- LTM3 %>% 
  full_join(midwifeconc) %>% 
  mutate(MIDWIFECONCERNC1 = case_when(MIDWIFECONCERNC1_R == 1 ~ 1,
                                   TRUE ~ MIDWIFECONCERNC1),
         MIDWIFECONCERNC2 = case_when(MIDWIFECONCERNC2_R == 1 ~ 1,
                                   TRUE ~ MIDWIFECONCERNC2),
         MIDWIFECONCERNC3 = case_when(MIDWIFECONCERNC3_R == 1 ~ 1,
                                   TRUE ~ MIDWIFECONCERNC3),
         MIDWIFECONCERNC4 = case_when(MIDWIFECONCERNC4_R == 1 ~ 1,
                                   TRUE ~ MIDWIFECONCERNC4),
         MIDWIFECONCERNC6 = case_when(MIDWIFECONCERNC6_R == 1 ~ 1,
                                   TRUE ~ MIDWIFECONCERNC6),
         MIDWIFECONCERNC7 = case_when(MIDWIFECONCERNC7_R == 0 ~ 0,
                                   TRUE ~ MIDWIFECONCERNC7))


LTM3 <- LTM3 %>% 
  left_join(Sources)
